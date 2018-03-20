{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Font
         ( testFont
         ) where

import           Imj.Prelude
import           Prelude(print, putStrLn)
import           Data.ByteString(ByteString)

import qualified Graphics.Rendering.FTGL as FTGL

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta.Backend.OpenGL

testFont :: IO ()
testFont = do
  let gameZ = gameGlyph 'Z'
      textZ = textGlyph 'Z'

      (_, idxGameZ) = decodeGlyph gameZ
      (_, idxTextZ) = decodeGlyph textZ
      fonts = mkFontsVariations
  void $ mapFonts fonts $ \content name ->
    createFonts (\_ -> withFont content name . createFont (Size 12 8) 0) >>= either
      error
      (\fts -> do
          putStrLn $ "font " ++ name ++ ":"
          print $ lookupFont idxGameZ fts
          --print $ lookupFont idxTextZ fts
          )

  let (content,name) = getFont 0 fonts
  createFonts (\_ -> withFont content name . createFont (Size 10 10) 0) >>= either
    error
    (\fts@(Fonts font0@(Font f0 _ _) font1@(Font f1 _ _)) -> do
        print $ lookupFont idxGameZ fts
        print $ lookupFont idxTextZ fts

        putStrLn =<< showDetailed font0
        putStrLn =<< showDetailed font1

        if f0 == f1
          then
            error "eq"
          else do
            print (f0,f1)
            _ <- FTGL.setFontFaceSize f1 10 72
            print =<< FTGL.getFontBBox f1 "j"
            print =<< FTGL.getFontBBox f1 "Z"
            _ <- FTGL.setFontFaceSize f1 20 72
            print =<< FTGL.getFontBBox f1 "j"
            print =<< FTGL.getFontBBox f1 "Z"
      )

mapFonts :: FontsVariations -> (ByteString -> String -> IO a) -> IO [a]
mapFonts fonts act =
  mapM (\i -> let (content,name) = getFont i fonts
              in act content name) [CycleFont 0..CycleFont (pred $ nFonts fonts)]
