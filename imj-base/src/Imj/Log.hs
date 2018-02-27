{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Log
        ( baseLog
        , keepExtremities
        , logDetailedException
        ) where


import           Imj.Prelude hiding(intercalate)

import           Control.Concurrent(myThreadId)
import           Data.List(length, lines)
import           Data.Text(pack, justifyRight, dropEnd)
import qualified Data.Text.Lazy.Builder as Builder(fromString)
import           Data.Text.Lazy(toStrict)
import           Data.Text.IO(putStrLn)
import           Data.Text.Lazy.Builder(toLazyText)
import           UnliftIO.Exception (SomeException(..))

import           Imj.Graphics.Color.Types

import           Imj.Timing
import           Imj.Graphics.Text.ColorString(ColorString, intercalate, colored, buildTxt)

{-# INLINE structure #-}
structure :: Text -> ColorString
structure = flip colored (gray 8)

baseLog :: ColorString -> IO ()
baseLog msg = do
  tid <- myThreadId
  t <- getSystemTime
  putStrLn $ toStrict $ toLazyText $ safeBuildTxt $
    intercalate (colored "|" white)
    [
    -- dropEnd 3 for microseconds precision
    colored (dropEnd 3 $ prettyShowTime t) white
  -- justify to avoid having big thread ids offset the logs alignment.
  -- drop 9 to skip 'ThreadId '
    , colored (justifyRight 6 ' ' $ pack $ drop 9 $ show tid) white
    , msg
    ]
 where
  -- restore white on black output in the end.
  safeBuildTxt colorStr =
    let (builder,color) = buildTxt Nothing colorStr
    in builder <> Builder.fromString (colorChange color whiteOnBlack)

logDetailedException :: (Show a)
                     => Maybe (Text, Text, [a])
                     -- ^ A list of values that will be added to the log.
                     -> SomeException
                     -> ColorString
logDetailedException infos e =
  contd <> intercalate contd (exceptionInfos ++ structure "Exception message:" : map exceptionMsg (lines $ show e))
 where
  contd = structure "\n (contd) "
  infoHeader (actionDesc, itemDesc, values) =
    structure $ "Exception while " <> actionDesc <> " " <> pack (show $ length values) <> " " <> itemDesc <> "(s):"
  infoBody itemDesc idx v =
    structure (" - " <> itemDesc <> " " <> pack (show idx) <> ":") <> keepExtremities (show v)
  exceptionMsg l =
    structure " | " <> colored (pack l) (rgb 5 1 1)
  exceptionInfos = maybe
    []
    (\i@(_, itemDesc, values) ->
      infoHeader i :
      zipWith (infoBody itemDesc) [0::Int ..] values)
    infos

keepExtremities :: String -> ColorString
keepExtremities msg =
  if l <= sz
    then
      inWhite msg
    else
      inWhite msg1 <>
      inGray (" *** truncated (" ++ show l ++ ") *** ") <>
      inWhite msg2
 where
  sz = 100
  !l = length msg
  sizeStart = quot sz 2
  sizeEnd = sz - sizeStart
  msg1 = take sizeStart msg
  msg2 = reverse $ take sizeEnd $ reverse msg
  inWhite x = colored (pack x) white
  inGray x = colored (pack x) (gray 12)