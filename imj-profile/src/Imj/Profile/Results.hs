{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Profile.Results
    ( toOptimalStrategies
    , addResult
    , MaybeResults
    , mkNothingResults
    , getResultsOfAllSizes
    , resultsToHtml
    , shouldTest
    ) where

import           Imj.Prelude hiding(div)

import           Data.Set(Set)
import qualified Data.IntMap.Strict as IMap
import qualified Data.List as List
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.String(IsString(..))
import           Data.Text(pack)
import           Text.Blaze.Html5 hiding(map)
import qualified Text.Blaze.Html5.Attributes as A

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.World.Space.Strategies
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Text.ColorString
import           Imj.Profile.Render.Characters
import           Imj.Profile.Result
import           Imj.Timing

-- Same has 'Results', except we have a 'Maybe' to express if the test has run yet or not.
newtype MaybeResults k = MaybeResults (Map SmallWorldCharacteristics (Maybe (Map (Maybe MatrixVariants) (TestDurations k Statistics))))
  deriving(Generic, Binary)
instance (NFData k) => NFData (MaybeResults k)

-- | Returns true if either
--
-- * there is no smaller size
-- * a valid result exists for the closest smaller size
-- * a valid result exists for any bigger size
--
-- It is used to discard tests that would fail with a hogh probability for a given timeout,
-- based on the knowledge of how tests with smaller or bigger sizes performed.
shouldTest :: SmallWorldCharacteristics -> MaybeResults k -> Bool
shouldTest world@(SWCharacteristics refSz _ _) m =
  let (l,biggersAndBiggers) =
        IMap.split 0 $ -- split removes the 0 key. It is ok because this key corresponds only
                       -- to the 'SmallWorldCharacteristics' passed as parameter.
        IMap.map catMaybes $ -- keep valid results only, i.e w can use 'null' to detect invalid results
        IMap.fromListWith (++) $ -- aggregate results that are at the same distance
        mapMaybe (\(sz,res) -> flip (,) [res] <$> homogenousDist refSz sz) $ -- discard size changes that are not homogenous
        Map.assocs $
        getResultsOfAllSizes world m
      smallersAndSmallers = IMap.toDescList l
      countValidBiggers = IMap.foldl' (\s -> (+) s . List.length) 0 biggersAndBiggers
  in case smallersAndSmallers of
        [] -> True
        closestSmallersValids:_ ->
          not (null closestSmallersValids) || countValidBiggers > 0

-- returns 'Nothing' when height and width change in opposite directions.
homogenousDist :: Size -> Size -> Maybe Int
homogenousDist (Size h w) (Size h' w')
  | dh > 0 && dw < 0 = Nothing
  | dh < 0 && dw > 0 = Nothing
  | otherwise = Just $ fromIntegral dh + fromIntegral dw
 where
  dw = w' - w
  dh = h' - h

getResultsOfAllSizes :: SmallWorldCharacteristics -> MaybeResults k -> Map Size (Maybe (Map (Maybe MatrixVariants) (TestDurations k Statistics)))
getResultsOfAllSizes (SWCharacteristics _ cc proba) (MaybeResults m) =
  Map.mapKeysMonotonic swSize $
  Map.filterWithKey
    (\(SWCharacteristics _ cc' proba') _ -> cc==cc' && proba == proba')
    m

-- | Note that the list of 'SmallWorldCharacteristics' passed here should contain all
-- possible future results added to the 'MaybeResults'. Hence, calling 'addResult'
-- with a 'SmallWorldCharacteristics' outside this set will error.
mkNothingResults :: Set SmallWorldCharacteristics -> MaybeResults k
mkNothingResults = MaybeResults . Map.fromSet (const Nothing)

-- | The 'Mayberesult' passed should contain a (possibly 'Nothing') result for 'SmallWorldCharacteristics'
{-# INLINABLE addResult #-}
addResult :: (Ord k) => SmallWorldCharacteristics -> Maybe MatrixVariants -> k -> TestStatus Statistics -> MaybeResults k -> MaybeResults k
addResult w strategy seed stats (MaybeResults res) =
  MaybeResults $ Map.alter
    (maybe
      (error $ "unforeseen result:" ++ show w)
      (Just . Just . maybe
        (Map.singleton strategy s)
        (Map.alter (Just . maybe s (mappend s)) strategy)))
    w
    res
 where
  s = TD $ Map.singleton seed stats

toOptimalStrategies :: MaybeResults k -> OptimalStrategies
toOptimalStrategies (MaybeResults m) = OptimalStrategies $
  Map.mapMaybe
    (maybe
      Nothing
      (Map.foldlWithKey'
        (\o strategy results -> case summarize results of
            NoResult -> o
            NTimeouts _ -> o
            FinishedAverage duration _ ->
              let cur = OptimalStrategy (fmap toVariantsSpec strategy) duration
              in Just $ maybe cur (min cur) o)
        Nothing))
    m

prettyProcessResult :: Maybe (Time Duration System)
                    -> SmallWorldCharacteristics
                    -> Map (Maybe MatrixVariants) (TestDurations k a)
                    -> [(Maybe (TestDurations k a), [ColorString])]
prettyProcessResult mayAllowedDt world results =
  let labelsAndEitherTimeoutsTimes = map
        (\(strategy, r) ->
          (colored (pack $ prettyShowMatrixVariants strategy) $ rgb 5 3 2, r))
        $ Map.assocs results
  in showTestResults mayAllowedDt -- map these lines to individual results
        labelsAndEitherTimeoutsTimes
        (colored (pack $ prettyShowSWCharacteristics world) $ rgb 3 4 2)

resultsToHtml :: (Show k)
              => Maybe (Time Duration System)
              -> MaybeResults k
              -> Html
resultsToHtml mayAllowedDt (MaybeResults results) =
  aggregate $
    concatMap
      (\(res,ls) ->
        let subHtml = fmap (subResultsAsHtml . showSubResult) res
        in map (flip (,) subHtml . toHtml) ls) $
      concatMap
        (uncurry $ prettyProcessResult mayAllowedDt) $
        map (fmap (fromMaybe Map.empty)) $
        Map.assocs results

 where

  showAsCs :: TestStatus Statistics -> [ColorString]
  showAsCs NotStarted = [colored "NotStarted" blue]
  showAsCs Timeout = [colored "Timeout" red]
  showAsCs (Finished _ stat) = map fromString $ prettyShowStats stat

  showSubResult :: (Show k)
                => TestDurations k Statistics
                -> [[ColorString]]
  showSubResult (TD m) =
    map (\(k, stat) ->
      colored' (pack $ show k) seedColor:
      showAsCs stat)
      $ Map.assocs m
   where
     seedColor = LayeredColor (gray 5) (gray 17)

  subResultsAsHtml :: [[ColorString]] -> Html
  subResultsAsHtml = mconcat . map subResultAsHtml

  subResultAsHtml :: [ColorString] -> Html
  subResultAsHtml lines =
    div $ do
      mconcat $
        map
          (\l -> p $ toHtml $ bool l "|" $ empty l)
          lines
      br

  aggregate :: [(Html, Maybe Html)] -> Html
  aggregate = div . mconcat . map (uncurry withDetail)


withDetail :: Html -> Maybe Html -> Html
withDetail mainResult =
  maybe
    (p mainResult)
    (\detail ->
      div
        ! A.class_ "clic"
        ! A.onclick (stringValue "toggle_details(this,event)")
        ! A.onmouseover (stringValue "show_overlay(this)")
        ! A.onmouseout (stringValue "hide_overlay(this)")
        -- No title (it is distracting)
        $ do
          div -- 0
            ! A.class_ "overlay" -- has absolute positionning, hence doesn't take space in flow.
            $ pure ()
          div -- 1
            mainResult
          div -- 2
            ! A.class_ "detail"
            $ do
              div br -- if br is outside this div, its height is not taken into account in toggleExpand
              detail)

--    ((!) p . A.title . stringValue . replaceNewlines)
{-  replaceNewlines [] = []
  replaceNewlines ('\n':xs) = chr 13:replaceNewlines xs
  replaceNewlines (c:xs)    = c     :replaceNewlines xs
  -}
  -- in a <p title="titleTxt"> ... </p>, onChrome, any of &#10; &#13; &#xA; in titleTxt make newlines.
