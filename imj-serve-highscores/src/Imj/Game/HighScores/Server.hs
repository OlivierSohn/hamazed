{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Imj.Game.HighScores.Server
  ( serveHighScores
  ) where

import           Imj.Prelude

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8(pack)
import           Data.ByteString.Lazy(fromStrict, toStrict)
import           Data.Binary(decode, encode)
import           Control.Exception (bracket)
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.LargeObjects
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment(lookupEnv)

import           Imj.Game.HighScores
import           Imj.Game.HighScores.API
import           Imj.Game.HighScores.Server.Args
import           Imj.Network

type DBConnectionString = ByteString

serveHighScoresApp :: Pool Connection -> Application
serveHighScoresApp conns = do

  serve highScoresAPI
       $ healthHandler
    :<|> getHighScoresHandler
    :<|> insertHighScoreHandler

 where

  getHighScoresHandler :: Handler HighScores
  getHighScoresHandler =
    maybe
      mkEmptyHighScores
      (\(b :: ByteString) -> decode $ fromStrict b)
      <$> (liftIO $
        withResource conns $ \conn ->
          withTransaction conn $
            query_ conn "SELECT fileoid FROM files" >>= \case
              [] -> return Nothing
              [oid] -> do
                fdescriptor <- loOpen conn (fromOnly oid) ReadMode
                sz <- loSeek conn fdescriptor SeekFromEnd 0
                void $ loSeek conn fdescriptor AbsoluteSeek 0
                by <- loRead conn fdescriptor sz
                return $ Just (by :: ByteString)
              oids@(_:_) -> fail $ "multiple oids:" ++ show oids)


  -- in a transaction, removes entries for that key and adds one.
  insertHighScoreHandler :: HighScore -> Handler HighScores
  insertHighScoreHandler highScore =
    liftIO $
      withResource conns $ \conn ->
        withTransaction conn $ do
          currentHighScores <- query_ conn "SELECT fileoid FROM files" >>= \case
            [] -> return mkEmptyHighScores
            [oid] -> do
              fdescriptor <- loOpen conn (fromOnly oid) ReadMode
              sz <- loSeek conn fdescriptor SeekFromEnd 0
              void $ loSeek conn fdescriptor AbsoluteSeek 0
              by <- loRead conn fdescriptor sz
              return $ decode $ fromStrict by
            oids@(_:_) -> fail $ "multiple oids:" ++ show oids
          destOid <- query_ conn "SELECT fileoid FROM files" >>= \case
            [] -> do
              oid <- loCreat conn
              void $ execute conn
                "INSERT INTO files VALUES (?)"
                (Only oid)
              return oid
            [oid] -> return $ fromOnly oid
            oids@(_:_) -> fail $ "multiple oids:" ++ show oids
          let newHighScores = insertScore highScore currentHighScores
          loOpen conn destOid WriteMode >>= \f ->
            -- returns the number of bytes written (errors are handled inside hence the returned value is always >= 0)
            void $ loWrite conn f (toStrict $ encode newHighScores)
          return newHighScores

  healthHandler :: Int -> Handler Int
  healthHandler i = return $ i + 1

-- | Reads the environment variable 'DB_CONN_STR' to.
serveHighScores :: IO ()
serveHighScores = withArgs parserSrvPort $ \portArg -> do
  p <- maybe (return defaultPort) (fmap unServerPort . getServerPort) portArg
  fmap pack <$> lookupEnv "DB_CONN_STR" >>= maybe
    (fail "The environment variable \"DB_CONN_STR\" doesn't exist.")
    (\connStr -> do
      initDB connStr
      initConnectionPool connStr >>=
        run p . serveHighScoresApp)

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

initDB :: DBConnectionString -> IO ()
initDB connstr =
  bracket (connectPostgreSQL connstr) close $ \conn ->
    void $ execute_ conn
      "CREATE TABLE IF NOT EXISTS files (fileoid oid)"
