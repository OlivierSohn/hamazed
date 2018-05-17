{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

module Imj.Network
    ( ClientName(..), unClientName
    , Proposed, Approved
    , ClientNameProposal(..)
    , checkName
    -- * MAC addresses
    , MAC
    , getMacAddresses
    )
    where

import           Imj.Prelude
import           Data.Bits(shiftL)
import           Data.Char (isPunctuation, isSpace)
import qualified Data.Set as Set
import           Data.Set(Set)
import           Data.String(IsString(..))
import           Data.Text(unpack)
import           GHC.Word(Word64, Word8(..))
import qualified Network.Info as N(MAC(..), mac)
import           Network.Info(getNetworkInterfaces)
import           Options.Applicative(str, option, long, help, readerError)

import           Imj.Arg.Class

data Proposed
  deriving(Generic)
data Approved
  deriving(Generic)

newtype ClientName a = ClientName Text
  deriving(Generic, Show, Binary, Eq, NFData, IsString, Ord)
unClientName :: ClientName a -> Text
unClientName (ClientName t) = t

class ClientNameProposal a where
  -- | When 'Nothing' is passed, it is an anonymous connection.
  -- you can chose to support anonymous connections or
  -- reject them.
  --
  -- When 'Just' is passed, you can check wether the given name is valid
  acceptConnection :: Maybe a -> Either Text ()
  extractName :: Maybe a -> ClientName Proposed

instance ClientNameProposal () where
  extractName = const $ ClientName "void"
  acceptConnection = const $ Right ()

instance ClientNameProposal (ClientName Proposed) where
  extractName = ClientName . maybe "Player" unClientName
  acceptConnection = maybe (Right ()) (fmap (const ()) . checkName)

checkName :: ClientName Proposed -> Either Text (ClientName Proposed)
checkName c@(ClientName txt)
  | any ($ name) [ null, any isPunctuation, any isSpace] =
      Left "Name cannot contain punctuation or whitespace, and cannot be empty"
  | otherwise =
      Right c
 where
  name = unpack txt

instance Arg (ClientName Proposed) where
  parseArg =
    Just $
      option connectId
        (  long "connectId"
        <> help (
        "[Client] The connection identifier used to connect to the server.")
        )
   where

    connectId =
      str >>= \case
        [] -> readerError $ "Encountered an empty connection id."
        name -> return $ fromString name

-- | Memory-efficient representation of a MAC address
newtype MAC = MAC Word64
  deriving(Generic, Binary, Eq, Ord, NFData, Show)

localhostMAC :: MAC
localhostMAC = MAC 0

toMAC :: N.MAC -> MAC
toMAC (N.MAC a@(W8# _) b@(W8# _) c@(W8# _) d@(W8# _) e@(W8# _) f@(W8# _)) =
  MAC $
    fromIntegral a +
    (fromIntegral b) `shiftL` 8 +
    (fromIntegral c) `shiftL` 16 +
    (fromIntegral d) `shiftL` 24 +
    (fromIntegral e) `shiftL` 32 +
    (fromIntegral f) `shiftL` 40

getMacAddresses :: IO (Set MAC)
getMacAddresses =
  Set.fromList . filter (/= localhostMAC) . map (toMAC . N.mac) <$> getNetworkInterfaces
