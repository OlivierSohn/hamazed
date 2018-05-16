{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Imj.Game.Internal.ArgParse
      ( parserGameArgs
      -- * Default values
      , defaultPPU
      , defaultPort
      ) where

import           Imj.Prelude
import           Prelude (toInteger)

import           Data.Char(toLower)
import qualified Data.List as List
import           Data.Proxy
import           Options.Applicative
                   (Parser, short, long, option, str, help, optional
                  , ReadM, readerError, (<*>), flag)
import           Options.Applicative.Types(Parser(..))
import           Text.Read(readMaybe)

import           Imj.Arg.Class
import           Imj.Server.Class
import           Imj.Server.Color
import           Imj.Geo.Discrete.Types
import           Imj.Game.Configuration
import           Imj.ServerView.Types

import           Imj.Game.Audio.Class
import           Imj.Game.Class
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta.Backend.OpenGL(PreferredScreenSize(..), mkFixedScreenSize)

toSks :: Proxy g -> Proxy (StatefullKeysT g)
toSks _ = Proxy

parserGameArgs :: GameLogic g
               => Proxy g -> Parser (GameArgs g)
parserGameArgs p = GameArgs
  <$> parserServerOnly
  <*> parserSrvName
  <*> parserSrvPort
  <*> parserSrvLogs
  <*> parserSrvColorScheme
  <*> parseConnectId
  <*> parserBackend
  <*> parserPPU
  <*> parserScreenSize
  <*> parserDebug
  <*> parserAudio

 where

  parserBackend  =
    if needsStatefullKeys (toSks p) p
      then
        NilP $ Just $ Just $ BackendType False OpenGLWindow -- The terminal backend doesn't support stateful keys.
      else
        optional
          (option backendArg
            (  long "render"
            <> short 'r'
            <> help (
            "[Client] 'console': play in the console. " ++
            "'opengl': play in an opengl window (default value)." ++
            renderHelp)
            ))


parserSrvColorScheme
  :: Parser (Maybe (ColorScheme))
parserSrvColorScheme =
  maybe (NilP $ Just Nothing) optional parseArg

parseConnectId
  :: GameLogic g
  => Parser (Maybe (ConnectIdT (ServerT g)))
parseConnectId =
  maybe (NilP $ Just Nothing) optional parseArg

parserAudio
  :: GameLogic g
  => Parser (Maybe (AudioT g))
parserAudio =
  maybe (NilP $ Just $ Just defaultAudio) optional parseArg


parserServerOnly :: Parser ServerOnly
parserServerOnly =
  flag (ServerOnly False) (ServerOnly True)
    (  long "serverOnly"
    <> short 's'
    <> help
    "Create - only - the server (no client). Incompatible with --serverName."
    )

parserSrvName :: Parser (Maybe ServerName)
parserSrvName =
  optional
    (option srvNameArg
       (  long "serverName"
       <> short 'n'
       <> help (
       "Connect to a server " ++
       "(use \"localhost\" to target your machine). Incompatible with --serverOnly."
       )))

parserSrvPort :: Parser (Maybe ArgServerPort)
parserSrvPort =
  optional
    (option srvPortArg
       (  long "serverPort"
       <> short 'p'
       <> help (
       "Listening port of the server to connect to, or to create. Can be a number or an environment variable. " ++
       "Default is " ++ show (toInteger defaultPort) ++ ".")
       ))

parserSrvLogs :: Parser (Maybe ServerLogs)
parserSrvLogs =
  optional
    (option srvLogsArg
       (  long "serverLogs"
       <> short 'l'
       <> help (
       "'none': no server logs. 'console': server logs in the console. " ++ -- TODO merge with -d
       "Default is 'none'. Incompatible with --serverName."
       )))

parserScreenSize :: Parser (Maybe PreferredScreenSize)
parserScreenSize =
  optional
    (option screenSizeArg
      (  long "screenSize"
      <> help (
      "[Client OpenGL] The size of the opengl window. 'full': fullscreen. " ++
      "'\"width height\"' : size in pixels. Default: \"600 1400\". "
      )
      ))

parserPPU :: Parser (Maybe PPU)
parserPPU =
  optional
    (option ppuArg
      (  long "ppu"
      <> help (
      "[Client OpenGL] The size of a game element, in pixels: " ++
      "'\"w h\"' where w,h are even and >= 4. Default: \"12 8\". "
      )
      ))


parserDebug :: Parser Debug
parserDebug =
  flag (Debug False) (Debug True)
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
                    ++ "\nAccepted values are 'none' and 'console'."

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
    "ascii"        -> return $ fromCli Console
    "console"      -> return $ fromCli Console
    "term"         -> return $ fromCli Console
    "terminal"     -> return $ fromCli Console
    "opengl"       -> return $ fromCli OpenGLWindow
    "win"          -> return $ fromCli OpenGLWindow
    "window"       -> return $ fromCli OpenGLWindow
    st -> readerError $ "Encountered an invalid render type:\n\t"
                    ++ show st
                    ++ "\nAccepted render types are 'console' and 'opengl'."
                    ++ renderHelp
 where
  fromCli = BackendType True

srvNameArg :: ReadM ServerName
srvNameArg =
  str >>= \s -> case map toLower s of
    [] -> readerError $ "Encountered an empty servername. Accepted names are ip address or domain name."
                     ++ renderHelp
    name -> return $ ServerName name

srvPortArg :: ReadM (ArgServerPort)
srvPortArg =
  str >>= \case
    [] -> readerError $ "Encountered an empty serverport."
                     ++ renderHelp
    name ->
      maybe
        (return $ EnvServerPort name)
        (return . NumServerPort . ServerPort)
          (readMaybe name)

defaultPort :: ServerPort
defaultPort = ServerPort 10052
