module NonBlockingIO
    ( tryGetChar
    ) where


import           Data.Bool( bool )

import           System.IO( getChar
                          , hReady
                          , stdin)


data PredictWillBlock a = PredictWillBlock {
    _predictBlockingWillBlock :: IO Bool
  , _predictBlockingCall :: IO a
}

getCharPredictWillBlock :: PredictWillBlock Char
getCharPredictWillBlock = PredictWillBlock (not <$> hReady stdin) getChar

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

tryGetNonBlocking :: PredictWillBlock a -> IO (Maybe a)
tryGetNonBlocking (PredictWillBlock willBlock call) = willBlock >>= bool (Just <$> call) (return Nothing)

tryGetChar :: IO (Maybe Char)
tryGetChar = tryGetNonBlocking getCharPredictWillBlock
