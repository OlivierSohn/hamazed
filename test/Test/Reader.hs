module Test.Reader where

import Control.Monad.Reader(ReaderT, runReaderT, ask, liftIO)

import Render
import Render.Delta

data Env = Env {
    _renderFuncs :: !RenderFunctions
}


writeChar :: Char -> ReaderT Env IO ()
writeChar c = do
  (Env (RenderFunctions render _ _ _)) <- ask
  liftIO $ render c (Coords 0 0) whiteOnBlack


doFlush :: ReaderT Env IO ()
doFlush = do
  (Env (RenderFunctions _ _ _ f)) <- ask
  liftIO f

testReader :: IO ()
testReader = do
  r <- newDefaultContext
  let f = mkRenderFunctions r
  let env = Env f
  runReaderT (writeChar 'c' >> doFlush) env
  runReaderT (writeChar 'd' >> doFlush) env
  runReaderT (writeChar 'e' >> doFlush) env
  putStrLn "" -- needed because the test engine deletes last line if there is no newline
