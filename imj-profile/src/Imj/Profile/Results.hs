{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Profile.Results
    ( toOptimalStrategies
    , addResult
    , mkEmptyResults
    , Results(..)
    , longestDuration
    , resultsToHtml
    ) where

import           Imj.Prelude hiding(div)

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
import           Imj.Graphics.Text.ColorString hiding(putStrLn)
import           Imj.Profile.Render.Characters
import           Imj.Profile.Result
import           Imj.Timing

newtype Results k = Results (Map SmallWorldCharacteristics (Map (Maybe MatrixVariants) (TestDurations k Statistics)))
  deriving(Binary)

mkEmptyResults :: Results k
mkEmptyResults = Results Map.empty

{-# INLINABLE addResult #-}
addResult :: (Ord k) => SmallWorldCharacteristics -> Maybe MatrixVariants -> k -> TestStatus Statistics -> Results k -> Results k
addResult w strategy seed stats (Results res) =
  Results $ Map.alter
    (Just . maybe
      (Map.singleton strategy s)
      (Map.alter (Just . maybe s (mappend s)) strategy))
    w
    res
 where
  s = TD $ Map.singleton seed stats

longestDuration :: Results k -> Maybe (Time Duration System)
longestDuration (Results m) =
  Map.foldr
    (flip $ Map.foldr (\e -> maxTime (getSummaryDuration $ summarize e)))
    Nothing
    m
 where
  maxTime Nothing Nothing = Nothing
  maxTime Nothing (Just t) = Just t
  maxTime (Just t) Nothing = Just t
  maxTime (Just t) (Just t') = Just $ max t t'

toOptimalStrategies :: Results k -> OptimalStrategies
toOptimalStrategies (Results m) = OptimalStrategies $
  Map.mapMaybe
    (Map.foldlWithKey'
      (\o strategy results -> case summarize results of
          NoResult -> o
          NTimeouts _ -> o
          FinishedAverage duration _ ->
            let cur = OptimalStrategy (fmap toVariantsSpec strategy) duration
            in Just $ maybe cur (min cur) o)
      Nothing)
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
              -> Results k
              -> Html
resultsToHtml mayAllowedDt (Results results) =
  aggregate $
    concatMap
      (\(res,ls) ->
        let subHtml = fmap (subResultsAsHtml . showSubResult) res
        in map (flip (,) subHtml . toHtml) ls) $
      concatMap
        (uncurry $ prettyProcessResult mayAllowedDt) $
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
  aggregate = mconcat . map (uncurry withDetail)


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
