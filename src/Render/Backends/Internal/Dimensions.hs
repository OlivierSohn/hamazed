module Render.Backends.Internal.Dimensions
        ( getDimensions
        , bufferSizeFromWH
        ) where


import           Data.Maybe(fromMaybe)
import           System.Console.Terminal.Size as Term (size, Window(..))

import           Render.Types

import           Data.Word( Word16, Word32 )


getDimensions :: Maybe RenderSize
              -> IO (Word16, Word16)
getDimensions r =
  case fromMaybe FollowsTerminalSize r of
    (Fixed (Col w) (Row h)) -> do
       let (w',h')
             | w < 0 = error "negative width are not allowed"
             | h < 0 = error "negative height are not allowed"
             | otherwise = (w,h)
       return (fromIntegral w',fromIntegral h')
    FollowsTerminalSize     -> do
      mayTermSize <- Term.size
      return $ maybe
        (300, 90) -- sensible default values
        (\(Term.Window h w) -> (w,h))
          mayTermSize


bufferSizeFromWH :: Word16 -> Word16 -> (Word16, Word16)
bufferSizeFromWH w' h' =
  let w = max 1 w'
      h = max 1 h'
      sz = fromIntegral w * fromIntegral h :: Word32
  -- indexed cells use a Word16 index so we can't exceed the Word16 maxBound
  in if sz > fromIntegral (maxBound :: Word16)
       then
         error $ "buffer size cannot be bigger than " ++ show (maxBound :: Word16) ++
            " : " ++ show (sz, w, h)
       else
         (fromIntegral sz, w)
