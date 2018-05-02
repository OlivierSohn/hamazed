{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Hamazed.Loop.Run
      ( run
      , produceEvent
      ) where

import           Imj.Prelude
import           Prelude (putStr, toInteger)

import           Control.Concurrent(forkIO, readMVar, newEmptyMVar)
import           Control.Exception (try)
import           Data.Char(toLower)
import qualified Data.List as List
import           Data.Maybe(isJust)
import           Data.Text(pack)
import           Network.Socket(withSocketsDo)
import           Options.Applicative
                  (ParserHelp(..), progDesc, fullDesc, info, header, execParserPure, prefs, helper
                  , showHelpOnError, short, long, option, str, help, optional
                  , ReadM, readerError, (<*>), switch)
import           Options.Applicative.Extra(handleParseResult, overFailure)
import qualified Options.Applicative.Help as Appli (red)
import           System.Environment(getArgs)
import           System.Info(os)
import           Text.Read(readMaybe)

import           Imj.Server.Class
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Logic
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.ServerView.Types
import           Imj.ServerView

import           Imj.Audio
import           Imj.Game.Network
import           Imj.Game.Run
import           Imj.Game.Color
import           Imj.Game.Network.Server
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta
import           Imj.Graphics.Render.Delta.Backend.OpenGL(PreferredScreenSize(..), mkFixedScreenSize)
import           Imj.Graphics.Text.ColorString hiding(putStr)
import           Imj.Graphics.Text.Render
import           Imj.Game.Network.ClientQueues
import           Imj.Log

{- | Runs the Hamazed game.

If you chose to run in the terminal, and your terminal window is too small, the
program will error and tell you what is the minimum window size to run the game.

The game
<https://ghc.haskell.org/trac/ghc/ticket/7353 doesn't run on Windows>.
-}
run :: IO ()
run = withSocketsDo $
  if os == "mingw32"
    then
      error $ "Windows is not currently supported"
      ++ " (https://ghc.haskell.org/trac/ghc/ticket/7353)."
    else
      withAudio
        runWithArgs

data BackendType =
    Console
  | OpenGLWindow
  deriving (Show)

runWithArgs :: IO ()
runWithArgs =
  join $ execParserPure (prefs showHelpOnError) parserInfo <$> getArgs >>=
    handleParseResult . overFailure (\ph -> ph {helpError = fmap Appli.red $ helpError ph})
 where
  parserInfo =
    info (helper <*> parser)
    (  fullDesc
    <> header (
       "**** imj-game-hamazed-exe runs the 'Hamazed' multiplayer game. " ++
       "Each client (player) is connected to a server."
      )
    <> progDesc (
       "If you want to: " ++
       "(1) Create just a server, use [--serverOnly] and optionally [--serverPort]. " ++
       "(2) Create just a client connected to an existing server, use [--serverName] and optionally [--serverPort]. " ++
       "(3) Create both a server and a client connected to it, use optionally [--serverPort]."
       ))
  parser =
    runWithBackend
      <$> switch
          (  long "serverOnly"
          <> short 's'
          <> help
          "Create - only - the server (no client). Incompatible with --serverName."
          )
      <*> optional
            (option srvNameArg
               (  long "serverName"
               <> short 'n'
               <> help (
               "Connect to a server " ++
               "(use \"localhost\" to target your machine). Incompatible with --serverOnly."
               )))
      <*> optional
            (option srvPortArg
               (  long "serverPort"
               <> short 'p'
               <> help (
               "Listening port number of the server to connect to, or to create. " ++
               "Default is " ++ show (toInteger defaultPort) ++ ".")
               ))
      <*> optional
            (option srvLogsArg
               (  long "serverLogs"
               <> short 'l'
               <> help (
               "'none': no server logs. 'console': server logs in the console. " ++ -- TODO merge with -d
               "Default is 'none'. Incompatible with --serverName."
               )))
      <*> optional
            (option srvColorSchemeArg
               (  long "colorScheme"
               <> short 'c'
               <> help (
               "Defines the colors of ships. Possible values are: " ++ descPredefinedColors ++ ", " ++
               "'rgb' | '\"r g b\"' where r,g,b are one of {0,1,2,3,4,5}, " ++
               "'time' to chose colors based on server start time. " ++
               "Default is 322 / \"3 2 2\". Incompatible with --serverName."
               )))
      <*> optional
            (option suggestedPlayerName
              (  long "playerName"
              <> help (
              "[Client] the name of the player you want to use. " ++
              "Default is \"Player\".")
              ))
      <*> optional
            (option backendArg
              (  long "render"
              <> short 'r'
              <> help (
              "[Client] 'console': play in the console. " ++
              "'opengl': play in an opengl window (default value)." ++
              renderHelp)
              ))
      <*> optional
            (option ppuArg
              (  long "ppu"
              <> help (
              "[Client OpenGL] The size of a game element, in pixels: " ++
              "'\"w h\"' where w,h are even and >= 4. Default: \"12 8\". "
              )
              ))
      <*> optional
            (option screenSizeArg
              (  long "screenSize"
              <> help (
              "[Client OpenGL] The size of the opengl window. 'full': fullscreen. " ++
              "'\"width height\"' : size in pixels. Default: \"600 1400\". "
              )
              ))
      <*> switch
            (  long "debug"
            <> short 'd'
            <> help
            "[Client] print debug infos in the terminal."
            )

renderHelp :: String
renderHelp =
  "\nAccepted synonyms of 'console' are 'ascii', 'term', 'terminal'." ++
  "\nAccepted synonyms of 'opengl' are 'win', 'window'."

srvLogsArg :: ReadM ServerLogs
srvLogsArg =
  str >>= \s -> case map toLower s of
    "none"    -> return NoLogs
    "console" -> return ConsoleLogs
    st -> readerError $ "Encountered an invalid server log type:\n\t"
                    ++ show st
                    ++ "\nAccepted render types are 'none' and 'console'."

screenSizeArg :: ReadM PreferredScreenSize
screenSizeArg = map toLower <$> str >>= \lowercase -> do
  let err msg = readerError $
       "Encountered an invalid screen size:\n\t" ++
         lowercase ++
         maybe [] ("\n" ++) msg ++
         "\nAccepted values are : 'full', '\"width height\"'"
      asScreenSize l = case catMaybes $ map readMaybe l of
        [x,y] -> either (err . Just) return $ mkFixedScreenSize (fromIntegral (x::Int)) (fromIntegral y)
        _ -> err Nothing
  case List.words lowercase of
    ["full"] -> return FullScreen
    [x, y] -> asScreenSize [x,y]
    _ -> err Nothing

ppuArg :: ReadM PPU
ppuArg = map toLower <$> str >>= \lowercase -> do
  let err msg = readerError $
       "Encountered an invalid ppu:\n\t" ++
       lowercase ++
       maybe [] (\txt -> "\n" ++ txt) msg ++
       "\nAccepted values are:" ++
       "\n - '\"x y\"' where x,y are even integers >= 4 (for example \"12 8\")"
      asPPU l = case catMaybes $ map readMaybe l of
        [x,y] -> either (err . Just) return $ mkUserPPU (fromIntegral (x::Int)) (fromIntegral y)
        _ -> err Nothing
  case List.words lowercase of
    [x, y] -> asPPU [x,y]
    _ -> err Nothing

defaultPPU :: PPU
defaultPPU = Size 12 8

backendArg :: ReadM BackendType
backendArg =
  str >>= \s -> case map toLower s of
    "ascii"        -> return Console
    "console"      -> return Console
    "term"         -> return Console
    "terminal"     -> return Console
    "opengl"       -> return OpenGLWindow
    "win"          -> return OpenGLWindow
    "window"       -> return OpenGLWindow
    st -> readerError $ "Encountered an invalid render type:\n\t"
                    ++ show st
                    ++ "\nAccepted render types are 'console' and 'opengl'."
                    ++ renderHelp

srvNameArg :: ReadM ServerName
srvNameArg =
  str >>= \s -> case map toLower s of
    [] -> readerError $ "Encountered an empty servername. Accepted names are ip address or domain name."
                     ++ renderHelp
    name -> return $ ServerName name

srvPortArg :: ReadM ServerPort
srvPortArg =
  str >>= \s -> case map toLower s of
    [] -> readerError $ "Encountered an empty serverport."
                     ++ renderHelp
    name ->
      maybe
        (error $ "invalid number : " ++ show name)
        (return . ServerPort)
          (readMaybe name)

suggestedPlayerName :: ReadM SuggestedPlayerName
suggestedPlayerName =
  str >>= \case
    [] -> readerError $ "Encountered an empty player name."
                     ++ renderHelp
    name -> return $ SuggestedPlayerName name

runWithBackend :: Bool
               -> Maybe ServerName
               -> Maybe ServerPort
               -> Maybe ServerLogs
               -> Maybe ColorScheme
               -> Maybe SuggestedPlayerName
               -> Maybe BackendType
               -> Maybe PPU
               -> Maybe PreferredScreenSize
               -> Bool
               -> IO ()
runWithBackend serverOnly maySrvName maySrvPort maySrvLogs mayColorScheme mayPlayerName maybeBackend mayPPU mayScreenSize debug = do
  let printServerArgs = putStr $ List.unlines $ showArray (Just ("Server Arg", ""))
        [ ("Server-only", show serverOnly)
        , ("Server name", show maySrvName)
        , ("Server port", show maySrvPort)
        , ("Server logs", show maySrvLogs)
        , ("Colorscheme", show mayColorScheme)
        ]
      printClientArgs = putStr $ List.unlines $ showArray (Just ("Client Arg", ""))
        [ ("Client Rendering", show maybeBackend)
        , ("PPU             ", show mayPPU)
        , ("Player name     ", show mayPlayerName)
        , ("Client Debug    ", show debug)
        ]
  printServerArgs
  when serverOnly $ do
    let conflict x = error $ "'--serverOnly' conflicts with '" ++
                          x ++ "' (these options are mutually exclusive)."
    when (isJust maySrvName)    $ conflict "--serverName"
    when (isJust mayPPU)        $ conflict "--ppu"
    when (isJust mayScreenSize) $ conflict "--screenSize"
    when (isJust mayPlayerName) $ conflict "--playerName"
    when (isJust maybeBackend)  $ conflict "--render"

  let srvPort = fromMaybe defaultPort maySrvPort
      srv = mkServer mayColorScheme maySrvLogs maySrvName (ServerContent srvPort Nothing)
      player = fromMaybe "Player" mayPlayerName
  newEmptyMVar >>= \ready ->
    if serverOnly
      then
        startServerIfLocal srv ready
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

        void $ forkIO $ try (startServerIfLocal srv ready) >>= \case
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
        queues <- startClient player srv :: IO (ClientQueues HamazedGame)
        case backend of
          Console ->
            newConsoleBackend >>= runWith debug queues srv player
          OpenGLWindow ->
            newOpenGLBackend "Hamazed"
              (fromMaybe defaultPPU mayPPU)
              (fromMaybe (FixedScreenSize $ Size 600 1400) mayScreenSize)
              >>= either error (runWith debug queues srv player)

defaultPort :: ServerPort
defaultPort = ServerPort 10052

mkServer :: Maybe ColorScheme -> Maybe ServerLogs -> Maybe ServerName -> ServerContent WorldParameters -> ServerView HamazedServer
mkServer color logs Nothing =
  mkLocalServerView (fromMaybe NoLogs logs) (fromMaybe (ColorScheme $ rgb 3 2 2) color)
mkServer Nothing Nothing (Just (ServerName n)) =
  mkDistantServerView (ServerName $ map toLower n)
mkServer _ (Just _) (Just _) =
  error "'--serverLogs' conflicts with '--serverName' (these options are mutually exclusive)."
mkServer (Just _) _ (Just _) =
  error "'--colorScheme' conflicts with '--serverName' (these options are mutually exclusive)."
