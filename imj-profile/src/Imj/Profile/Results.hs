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
    , addRefinedResult
    , addUnrefinedResult
    , addResult'
    , MaybeResults(..)
    , TaggedResult(..) -- for white box tests
    , OldMaybeResults
    , mkNothingResults
    , mkNothingResults'
    , getResultsOfAllSizes
    , resultsToHtml
    , resultsToHtml'
    , shouldTest
    , canonicalize
    , homogenousDist
    ) where

import           Imj.Prelude hiding(div)

import           Data.Set(Set)
import           Data.IntMap.Internal(IntMap(..), Key)
import qualified Data.IntMap.Strict as IMap
import qualified Data.List as List
import           Data.Map.Internal(Map)
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

newtype OldMaybeResults k = OldMaybeResults (Map SmallWorldCharacteristics (Map (Maybe MatrixVariants) (TestDurations k Statistics)))
  deriving(Generic, Binary)
instance (NFData k) => NFData (OldMaybeResults k)

-- Same has 'Results', except we have a single variant and we tag the results.
newtype MaybeResults k = MaybeResults (Map SmallWorldCharacteristics (Maybe (TaggedResult k Statistics)))
  deriving(Generic, Binary)
instance (NFData k) => NFData (MaybeResults k)

-- This is close to 'OptimalStrategy', but more detailed
data TaggedResult k a = TaggedResult {
    _resultTag :: !StrategyTag
  , _resultVariant :: !(Maybe MatrixVariantsSpec)
  , _result :: !(TestDurations k a)
}
  deriving(Generic, Show)
instance (NFData k, NFData a) => NFData (TaggedResult k a)
instance (Binary k, Binary a) => Binary (TaggedResult k a)

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
  let (smaller,bigger) =
        IMap.split 0 $ -- split removes the 0 key. It is ok because this key corresponds only
                       -- to the 'SmallWorldCharacteristics' passed as parameter.
        IMap.map catMaybes $ -- keep valid results only, i.e w can use 'null' to detect invalid results
        IMap.fromListWith (++) $ -- aggregate results that are at the same distance
        mapMaybe (\(sz,res) -> flip (,) [res] <$> homogenousDist refSz sz) $ -- discard size changes that are not homogenous
        Map.assocs $
        getResultsOfAllSizes world m
      countValidBiggers = IMap.foldl' (\s -> (+) s . List.length) 0 bigger
  in maybe
      True
      (\(_,closestSmallersValids) -> case closestSmallersValids of
        [] -> countValidBiggers > 0
        _:_ -> True)
      $ lookupMax smaller

lookupMax :: IntMap a -> Maybe (Key, a) -- TODO remove once available in container
lookupMax Nil = Nothing
lookupMax (Tip k v) = Just (k,v)
lookupMax (Bin _ m l r)
  | m < 0     = go l
  | otherwise = go r
    where go (Tip k v)      = Just (k,v)
          go (Bin _ _ _ r') = go r'
          go Nil            = Nothing

canonicalize :: Size -> Size
canonicalize sz@(Size (Length h) (Length w))
  | h <= w = sz
  | otherwise = Size (fromIntegral w) (fromIntegral h)

-- returns 'Nothing' when height and width change in opposite directions.
homogenousDist :: Size -> Size -> Maybe Int
homogenousDist s1 s2 = hDist (canonicalize s1) (canonicalize s2)
 where
  hDist (Size h w) (Size h' w')
    | dh > 0 && dw < 0 = Nothing
    | dh < 0 && dw > 0 = Nothing
    | otherwise = Just $ fromIntegral dh + fromIntegral dw
   where
    dw = w' - w
    dh = h' - h

getResultsOfAllSizes :: SmallWorldCharacteristics -> MaybeResults k -> Map Size (Maybe (TaggedResult k Statistics))
getResultsOfAllSizes (SWCharacteristics _ cc proba) (MaybeResults m) =
  Map.mapKeysMonotonic swSize $
  Map.filterWithKey
    (\(SWCharacteristics _ cc' proba') _ -> cc==cc' && proba == proba')
    m

-- | Note that the list of 'SmallWorldCharacteristics' passed here should contain all
-- possible future results added to the 'MaybeResults'. Hence, calling 'addResult'
-- with a 'SmallWorldCharacteristics' outside this set will error.
mkNothingResults' :: Set SmallWorldCharacteristics -> OldMaybeResults k
mkNothingResults' = OldMaybeResults . Map.fromSet (const Map.empty)

mkNothingResults :: Set SmallWorldCharacteristics -> MaybeResults k
mkNothingResults = MaybeResults . Map.fromSet (const Nothing)

{-# INLINABLE addRefinedResult #-}
addRefinedResult :: SmallWorldCharacteristics -> Maybe MatrixVariantsSpec -> Map k (TestStatus Statistics) -> MaybeResults k -> MaybeResults k
addRefinedResult w strategy m (MaybeResults res) =
  MaybeResults $ Map.alter
    (maybe
      (error $ "unforeseen result:" ++ show w)
      (Just . Just . maybe
        add
        (\(TaggedResult tag _ _) -> case tag of
            Unrefined _ -> add
            Refined -> error "would overwrite a refined result")))
    w
    res
 where
  add = TaggedResult Refined strategy $ TD m

{-# INLINABLE addUnrefinedResult #-}
addUnrefinedResult :: (Show k) => SmallWorldCharacteristics -> Maybe MatrixVariantsSpec -> [Maybe MatrixVariantsSpec] -> Map k (TestStatus Statistics) -> MaybeResults k -> MaybeResults k
addUnrefinedResult w strategy remaining m (MaybeResults res) =
  MaybeResults $ Map.alter
    (maybe
      (error $ "unforeseen result:" ++ show w)
      (Just . Just . maybe
        add
        (\r -> error $ "would overwrite:" ++ show r)))
    w
    res
 where
  add = TaggedResult (Unrefined remaining) strategy $ TD m

{-# INLINABLE addResult' #-}
addResult' :: (Ord k) => SmallWorldCharacteristics -> Maybe MatrixVariants -> k -> TestStatus Statistics -> OldMaybeResults k -> OldMaybeResults k
addResult' w strategy seed stats (OldMaybeResults res) =
  OldMaybeResults $ Map.alter
    (Just . maybe
      (Map.singleton strategy s)
      (Map.alter (Just . maybe s (mappend s)) strategy))
    w
    res
 where
  s = TD $ Map.singleton seed stats

toOptimalStrategies :: MaybeResults k -> OptimalStrategies
toOptimalStrategies (MaybeResults m) = OptimalStrategies $
  Map.mapMaybe
    (maybe
      Nothing
      (\(TaggedResult _ strategy results) -> case summarize results of
            NoResult -> Nothing
            NTimeouts _ -> Nothing
            FinishedAverage duration _ ->
              Just $ OptimalStrategy strategy duration))
    m

prettyProcessResult' :: Maybe (Time Duration System)
                    -> SmallWorldCharacteristics
                    -> Map (Maybe MatrixVariants) (TestDurations k a)
                    -> [(Maybe (TestDurations k a), [ColorString])]
prettyProcessResult' mayAllowedDt world results =
  let labelsAndEitherTimeoutsTimes = map
        (\(strategy, r) ->
          (colored (pack $ prettyShowMatrixVariants strategy) $ rgb 5 3 2, r))
        $ Map.assocs results
  in showTestResults mayAllowedDt -- map these lines to individual results
        labelsAndEitherTimeoutsTimes
        (colored (pack $ prettyShowSWCharacteristics world) $ rgb 3 4 2)


prettyProcessResult :: Maybe (Time Duration System)
                    -> SmallWorldCharacteristics
                    -> Maybe (TaggedResult k a)
                    -> [(Maybe (TestDurations k a), [ColorString])]
prettyProcessResult mayAllowedDt world results =
  let labelsAndEitherTimeoutsTimes =
        maybe
          []
          (\(TaggedResult tag strategy r) ->
            [(colored (pack $ unwords [show tag, prettyShowMatrixVariantsSpec strategy]) $ rgb 5 3 2, r)])
          results
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
        Map.assocs results

resultsToHtml' :: (Show k)
              => Maybe (Time Duration System)
              -> OldMaybeResults k
              -> Html
resultsToHtml' mayAllowedDt (OldMaybeResults results) =
  aggregate $
    concatMap
      (\(res,ls) ->
        let subHtml = fmap (subResultsAsHtml . showSubResult) res
        in map (flip (,) subHtml . toHtml) ls) $
      concatMap
        (uncurry $ prettyProcessResult' mayAllowedDt) $
        Map.assocs results

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
