module Env(
         Env(..)
       , RenderFunctions(..)
       -- * delta rendering functions
       , drawTxt
       , drawChars
       , drawChar
       , flush
       -- * reexports
       , ReaderT
       , ask
       , liftIO
       ) where

import           Control.Monad.Reader(ReaderT, ask, liftIO)
import           Color.Types
import           Geo.Discrete.Types
import           Data.Text(Text)

import Render(RenderFunctions(..))

newtype Env = Env {
    _envRenderFuncs :: RenderFunctions
}


{-# INLINE drawTxt #-}
drawTxt :: Text -> Coords -> LayeredColor -> ReaderT Env IO ()
drawTxt txt co la = do
  (Env (RenderFunctions _ _ r _)) <- ask
  liftIO $ r txt co la

{-# INLINE drawChars #-}
drawChars :: Int -> Char -> Coords -> LayeredColor -> ReaderT Env IO ()
drawChars i c co la = do
  (Env (RenderFunctions _ r _ _)) <- ask
  liftIO $ r i c co la

{-# INLINE drawChar #-}
drawChar :: Char -> Coords -> LayeredColor -> ReaderT Env IO ()
drawChar c co la = do
  (Env (RenderFunctions r _ _ _)) <- ask
  liftIO $ r c co la

{-# INLINE flush #-}
flush :: ReaderT Env IO ()
flush = do
  (Env (RenderFunctions _ _ _ fl)) <- ask
  liftIO fl
