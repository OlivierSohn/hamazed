{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Intent
    ( UserIntent(..)
    , firstNonReport
    , mkTerminator
    , onReport
    ) where

import           Imj.Prelude

import           Prelude(getChar)
import           Control.Concurrent(forkIO, throwTo, myThreadId, threadDelay)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Exception(Exception(..))
import           Data.Text(pack)
import           System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)
import           System.Timeout(timeout)
import           System.IO(hSetBuffering, stdin, BufferMode(..))

import           Imj.Graphics.Color
import qualified Imj.Graphics.Text.ColorString as CS

data UserIntent =
    Run
  | Pause !UserIntent
  | Cancel
  | Report !UserIntent
  deriving(Generic, Show)
instance NFData UserIntent

firstNonReport :: UserIntent -> UserIntent
firstNonReport (Report x) = firstNonReport x
firstNonReport y = y


data TestException = TestInterruptedByUser
    deriving Show
instance Exception TestException

setToFalseOnTermination :: MVar UserIntent -> IO ()
setToFalseOnTermination intent = do
  mainTid <- myThreadId
  mapM_ (installHandlers mainTid) [sigINT, sigTERM]
 where
  installHandlers mainTid sig =
    void $ installHandler sig (Catch $ handleTermination mainTid) Nothing

  handleTermination mainTid =
    modifyMVar_ intent $ \case
      Cancel -> do
        CS.putStrLn $ CS.colored
          "\nUser forced test termination."
          red
        threadDelay 100000
        void $ throwTo mainTid TestInterruptedByUser
        return Cancel
      _ -> do
        CS.putStrLn $ CS.colored
          "\nUser requested test termination. A report will be generated, unless user requests termination one more time."
          orange
        return Cancel

mkTerminator :: IO (MVar UserIntent)
mkTerminator = do
  b <- newMVar Run
  void $ forkIO $ do
    hSetBuffering stdin NoBuffering

    let skipRepeats = void $ timeout 1000000 $ forever $ do
          void getChar
          CS.putStrLn $ CS.colored "\nA key-press was ignored. Please try again in a second." yellow

    forever $ getChar >>= \case
      'r' -> do
        CS.putStrLn $ CS.colored "\nAn intermediate html report will be generated as soon as possible..." yellow
        modifyMVar_ b $ \prevIntent -> return $ Report prevIntent
        skipRepeats
      ' ' -> do
        modifyMVar_ b $ \case
          Pause x -> do
            CS.putStrLn $ CS.colored ("\nTest state is " <> pack (show x)) yellow
            return x
          s -> do
            CS.putStrLn $ CS.colored "\nTest will pause as soon as possible, please wait..." yellow
            return $ Pause s
        skipRepeats
      _ -> return ()
  setToFalseOnTermination b
  return b


onReport :: (UserIntent -> IO ()) -> MVar UserIntent -> IO ()
onReport f intent = readMVar intent >>= \case
  i@(Report prevIntent) -> do
    f i
    modifyMVar_ intent $ const $ return $ firstNonReport prevIntent
  _ -> return ()
