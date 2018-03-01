{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Font
         ( testFont
         ) where

import           Imj.Prelude
import           Prelude(print, putStrLn)

import qualified Graphics.Rendering.FTGL as FTGL

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Font

testFont :: IO ()
testFont = do
  let gameZ = gameGlyph 'Z'
      textZ = textGlyph 'Z'

      (_, idxGameZ) = decodeGlyph gameZ
      (_, idxTextZ) = decodeGlyph textZ

  createFonts 0 (Coords 10 10) >>= either
    error
    (\fonts@(Fonts font0@(Font f0 _) font1@(Font f1 _)) -> do
        print $ lookupFont idxGameZ fonts
        print $ lookupFont idxTextZ fonts

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
