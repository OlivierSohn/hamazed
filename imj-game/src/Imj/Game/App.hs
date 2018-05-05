{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Imj.Game.App
      ( runGame
      ) where

import           Imj.Prelude
import           Prelude (putStr)

import           Control.Concurrent(forkIO, readMVar, newEmptyMVar)
import           Control.Exception (try)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State(runStateT)
import           Data.Char(toLower)
import qualified Data.List as List
import           Data.Maybe(isJust)
import           Data.Proxy
import           Data.Text(pack)
import           Network.Socket(withSocketsDo)
import           Options.Applicative
                  (ParserHelp(..), Parser, progDesc, fullDesc, info, header, execParserPure, prefs, helper
                  , showHelpOnError, (<*>))
import           Options.Applicative.Extra(handleParseResult, overFailure)
import qualified Options.Applicative.Help as Appli (red)
import           System.Environment(getArgs, getProgName)
import           System.Info(os)

import           Imj.Audio
import           Imj.Game.Exceptions
import           Imj.Game.Env
import           Imj.Game.Configuration
import           Imj.Game.Internal.ArgParse
import           Imj.Game.Network
import           Imj.Game.Network.ClientQueues
import           Imj.Game.KeysMaps
import           Imj.Game.Loop
import           Imj.Game.State
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta
import           Imj.Graphics.Render.Delta.Backend.OpenGL(PreferredScreenSize(..))
import           Imj.Graphics.Screen
import           Imj.Graphics.Text.RasterizedString
import           Imj.Graphics.Text.ColorString hiding(putStr)
import           Imj.Graphics.Text.Render
import           Imj.Input.Types
import           Imj.Log
import           Imj.Server.Class
import           Imj.Server.Color
import           Imj.ServerView.Types
import           Imj.ServerView

{- | Runs the game.

The game <https://ghc.haskell.org/trac/ghc/ticket/7353 doesn't run on Windows>.
-}
runGame :: (GameLogic g) => Proxy g -> IO ()
runGame p = runOnPlatform p $ run p

runOnPlatform :: GameLogic g => Proxy g -> (GameArgs g -> IO a) -> IO a
runOnPlatform p app = withSocketsDo $
  if os == "mingw32"
    then
      error $
        "Windows is not currently supported:" ++
        "https://ghc.haskell.org/trac/ghc/ticket/7353"
    else
      withArgs (parserGameArgs p) app

withArgs :: Parser x -> (x -> IO a) -> IO a
withArgs parser app = do
  progn <- getProgName
  join $ execParserPure (prefs showHelpOnError) (parserInfo progn) <$> getArgs >>=
    handleParseResult . overFailure (\ph -> ph {helpError = fmap Appli.red $ helpError ph})
 where
  parserInfo n =
    info (helper <*> (app <$> parser))
    (  fullDesc
    <> header (
       "**** " ++ n ++ " runs a multiplayer client/server game."
      )
    <> progDesc (
       "If you want to: " ++
       "(1) Create just a game server, use [--serverOnly] and optionally [--serverPort]. " ++
       "(2) Create just a client connected to an existing game server, use [--serverName] and optionally [--serverPort]. " ++
       "(3) Create both a game server and a client connected to it, use optionally [--serverPort]."
       ))

run :: GameLogic g => Proxy g -> GameArgs g -> IO ()
run prox
  (GameArgs
    (ServerOnly serverOnly)
    maySrvName maySrvPort maySrvLogs mayConfig mayConnectId maybeBackend mayPPU mayScreenSize debug useAudio) = do
  let printServerArgs = putStr $ List.unlines $ showArray (Just ("Server Arg", ""))
        [ ("Server-only", show serverOnly)
        , ("Server name", show maySrvName)
        , ("Server port", show maySrvPort)
        , ("Server logs", show maySrvLogs)
        , ("Colorscheme", show mayConfig)
        ]
      printClientArgs = putStr $ List.unlines $ showArray (Just ("Client Arg", ""))
        [ ("Client Rendering", show maybeBackend)
        , ("PPU             ", show mayPPU)
        , ("Player name     ", show mayConnectId)
        , ("Client Debug    ", show debug)
        , ("Client Audio    ", show useAudio)
        ]
  printServerArgs
  when serverOnly $ do
    let conflict x = error $ "'--serverOnly' conflicts with '" ++
                          x ++ "' (these options are mutually exclusive)."
    when (isJust maySrvName)    $ conflict "--serverName"
    when (isJust mayPPU)        $ conflict "--ppu"
    when (isJust mayScreenSize) $ conflict "--screenSize"
    when (isJust mayConnectId) $ conflict "--connectId"
    when (isJust maybeBackend)  $ conflict "--render"
  when (isJust mayConfig && isJust maySrvName) $
    error "'--colorScheme' conflicts with '--serverName' (these options are mutually exclusive)."
  when (isJust maySrvLogs && isJust maySrvName) $
    error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."

  let srvPort = fromMaybe defaultPort maySrvPort
      srv = mkServer maySrvName mayConfig maySrvLogs (ServerContent srvPort Nothing)
  newEmptyMVar >>= \ready -> do
    let srvIfLocal = startServerIfLocal prox srv ready
    if serverOnly
      then
        srvIfLocal
      else do
        printClientArgs
        let backend = fromMaybe OpenGLWindow maybeBackend
        case backend of
          Console -> do
            when (isJust mayPPU) $
              error $ "Cannot use --ppu with the console backend. " ++
                "Please use the opengl backend instead, or remove --ppu from the command line."
            when (isJust mayScreenSize) $
              error $ "Cannot use --screenSize with the console backend. " ++
                "Please use the opengl backend instead, or remove --screenSize from the command line."
          _ -> return ()

        void $ forkIO $ try srvIfLocal >>= \case
          -- when exceptions propagate past forkIO, they are reported to stderr:
          -- https://ghc.haskell.org/trac/ghc/ticket/3628
          -- That is annoying when playing in the terminal, so we mute "normal" exceptions.
          Left (_ :: GracefulProgramEnd) -> return ()
          Right () -> return ()

        readMVar ready >>= either
          (\e -> baseLog (colored (pack e) red) >> error e)
          (baseLog . flip colored chartreuse . pack)
        -- the listening socket is available, we can continue.

        -- The type here determines which game we are playing.
        queues <- startClient prox mayConnectId srv
        case backend of
          Console ->
            newConsoleBackend >>= runWith useAudio debug queues srv mayConnectId
          OpenGLWindow ->
            newOpenGLBackend (gameName prox)
              (fromMaybe defaultPPU mayPPU)
              (fromMaybe (FixedScreenSize $ Size 600 1400) mayScreenSize)
              >>= either error (runWith useAudio debug queues srv mayConnectId)

mkServer :: Maybe ServerName
         -> Maybe ColorScheme
         -> Maybe ServerLogs
         -> ServerContent (ValuesT s)
         -> ServerView s
mkServer Nothing conf logs =
  mkLocalServerView (fromMaybe NoLogs logs) conf
mkServer (Just (ServerName n)) _ _ =
  mkDistantServerView (ServerName $ map toLower n)


{-# INLINABLE runWith #-}
runWith :: (GameLogic g
          , PlayerInput i, DeltaRenderBackend i)
        => WithAudio
        -> Debug
        -> ClientQueues g
        -> ServerView (ServerT g)
        -> Maybe (ConnectIdT (ServerT g))
        -> i
        -> IO ()
runWith au@(WithAudio useAudio) debug queues srv player backend =
  withTempFontFile font fontname $ \path -> withFreeType $ withSizedFace path (Size 16 16) $ \face ->
    flip withDefaultPolicies backend $ \drawEnv -> do
      screen <- mkScreen <$> getDiscreteSize backend
      env <- mkEnv drawEnv backend queues face au
      let mayAudio = if useAudio then withAudio else id
      void $ createState screen debug player srv NotConnected >>=
        mayAudio . runStateT (runReaderT (loop translatePlatformEvent onEvent) env)

 where

  (font,fontname) = fromMaybe (error "absent font") $ look "LCD" fontFiles

  look _ [] = Nothing
  look name ((f,ftName,_):rest) =
    if ftName == name
      then Just (f,ftName)
      else look name rest
