{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

  {- | A 'ColorString' is a multicolored 'Text'.-}

module Imj.Graphics.Text.ColorString
            (
            -- * Type
              ColorString(..)
            -- * Constructors
{- | 'colored' creates a 'ColorString' using the specified foreground color on
/black/ background, wherease 'colored'' allows you to chose both the
background and the foreground colors.

And since 'ColorString' is 'Monoid', we can write:

@
str = colored \"Hello\" white <> colored \" World\" yellow
@
 -}
            , colored
            , colored'
            , concat
            , intercalate
            , replaceBackground
            -- * Utilities
            , countChars
            , take
            , destructure
            -- * Convert to colored Text
            , buildTxt
            , safeBuildTxt
            -- * IO
            , putStrLn
            , putStr
            -- * Blaze helpers
            , colorAttribute
            -- * Reexports
            , LayeredColor(..)
            ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(asks)
import           Data.Char(isSpace, chr)
import           Data.String(IsString(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text(putStrLn, putStr)
import           Data.Text.Lazy(toStrict)
import           Data.Text.Lazy.Builder(Builder, toLazyText)
import qualified Data.Text.Lazy.Builder as Builder(fromText, fromString)
import qualified Data.List as List(length, concat, splitAt)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5(ToMarkup(..))

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Font
import           Imj.Graphics.Text.ColorString.Interpolation
import           Imj.Util

newtype ColorString = ColorString [(Text, LayeredColor)] deriving(Show, Generic, Eq)
instance Monoid ColorString where
  mempty = ColorString []
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y
-- we can't use the Generic one because of missing instance for 'Text'
instance IsString ColorString where
  fromString str = ColorString [(Text.pack str, whiteOnBlack)]
instance PrettyVal ColorString where
  prettyVal c = prettyVal $ map fst $ destructure c
-- | Safari doesn't seem to handle &nbsp; well, but it works ok for Chrome + string renderer.
instance ToMarkup ColorString where
  toMarkup cs = do
    let (ColorString x) = restructure $ transformConsecutiveSpaces $ destructure cs
    mconcat <$> forM x (\(txt, color) ->
      H.span
        H.! A.style (colorAttribute color) $ H.text txt)
   where
    transformConsecutiveSpaces l =
      go l False
     where
      go ((' ',col):xs) True = (chr 160,col) : go xs True
      -- The version below works well for Chrome + string renderer, but not for
      --    Chrome + pretty renderer, where some spaces are added.
      --go ((' ',col):xs) False = (chr 160,col) : go xs True
      -- Same as version above, except that for Chrome + pretty renderer,
      --   /less/ spaces are added, hence it's a little better.
      go (x@(' ',_):xs) False = x : go xs True
      go (('-',col):xs) _ = (chr 8209,col) : go xs False -- transform hyphens into non-breaking hyphens.
      go (x:xs) _ = x : go xs False
      go [] _ = []

instance Characters ColorString where
  length = countChars
  empty (ColorString x) = null x || all (Text.null . fst) x

  splitAt idx (ColorString l) =
    (ColorString $ reverse left
   , ColorString $ reverse right)
    where
      (left, right) = split' idx l ([],[])
      split' :: Int
             -> [(Text,LayeredColor)]
             -> ([(Text,LayeredColor)], [(Text,LayeredColor)])
             -> ([(Text,LayeredColor)], [(Text,LayeredColor)])
      split' _ [] cur = cur
      split' n (t@(txt,color):xs) (lefts,rights)
        | n >= len  = split' (n-len) xs (            t:lefts,                rights)
        | n > 0     = split' 0       xs ((tLeft,color):lefts, (tRight,color):rights)
        | otherwise = split' 0       xs (              lefts,              t:rights)
        where
          len = Text.length txt
          (tLeft,tRight) = Text.splitAt n txt

  cons c (ColorString l) = ColorString $ (Text.singleton c, whiteOnBlack):l

  intercalate (ColorString i) =
    ColorString . List.concat . intersperse' i . map (\(ColorString s) -> s)

  take nChars (ColorString l)
    | nChars <= 0 = mempty
    | otherwise = ColorString $ reverse $ take' nChars [] l
    where
      take' :: Int -> [(Text, LayeredColor)] -> [(Text, LayeredColor)] -> [(Text, LayeredColor)]
      take' _ cur [] = cur
      take' 0 cur _  = cur
      take' n cur (r@(txt,color):rs)
        | n >= lr = take' (n-lr) (r:cur) rs
        | otherwise = (Text.take n txt, color):cur
        where lr = Text.length txt

  concat = ColorString . concatMap (\(ColorString s) -> s)

  unwords strs =
    unwords' (reverse strs) mempty
   where
    unwords' [] cur = cur
    unwords' (x:xs) (ColorString []) =
      unwords' xs x
    unwords' (ColorString x:xs) (ColorString ((c,color):cs)) =
      unwords' xs $ ColorString $ x ++ ((Text.cons ' ' c,color):cs)

  words (ColorString l) =
    map ColorString $ reverse $ words' l False []
   where
    words' :: [(Text,LayeredColor)] -> Bool -> [[(Text,LayeredColor)]] -> [[(Text,LayeredColor)]]
    words' [] _ cur = cur
    words' ((txt,color):xs) prevShouldMergeLastWord cur =
      case Text.words txt of
        [] ->
          words' xs prevShouldMergeLastWord cur
        asWords@(firstWord:lastWords) ->
          words' xs (not $ isSpace $ Text.last txt) cur'
         where
          cur' =
            if not (isSpace $ Text.head txt) && prevShouldMergeLastWord
              then
                -- we merge this iteration's first word with the previous iteration's
                -- last word.
                case cur of
                  [] -> error "prevShouldMergeLastWord is True, cur cannot be empty"
                  c:cs -> map (\aw -> [(aw, color)]) (reverse lastWords)
                          ++ ((c ++ [(firstWord, color)]) : cs)
              else
                -- we don't merge words
                map (\aw -> [(aw, color)]) (reverse asWords) ++ cur

  colorize color (ColorString l) =
    ColorString $ map (\(t, _) -> (t, color)) l

  drawOnPath positions (ColorString cs) = do
    f <- asks drawGlyph'
    let go [] _ = return ()
        go _ [] = return ()
        go ps ((txt, color):rest) = do
          let len = length txt
              (headRs, tailRs) = List.splitAt len $ assert (List.length ps >= len) ps
          zipWithM_ (\char coord -> f (textGlyph char) coord color) (Text.unpack txt) headRs
          go tailRs rest
    go positions cs
  {-# INLINABLE drawOnPath #-}
  {-# INLINABLE concat #-}
  {-# INLINABLE intercalate #-}
  {-# INLINABLE take #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
  {-# INLINABLE empty #-}
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
instance Positionable ColorString where
  drawAt (ColorString cs) pos = do
    f <- asks drawTxt'
    foldM_
      (\count (txt, color) -> do
        let l = length txt
        f txt (move count RIGHT pos) color
        return $ count + l
      ) 0 cs
  width = fromIntegral . countChars
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
-- | First interpolating characters, then color.
instance DiscreteDistance ColorString where
  distance c1 c2 =
    let colorDist (_, color) (_, color') = distance color color'
        n1 = countChars c1
        n2 = countChars c2
        s1 = destructure c1
        s2 = destructure c2

        (c1', remaining) = interpolateChars s1 s2 countTextChanges
        s1' = assert (remaining == 0) c1'
        l = zipWith colorDist s1' s2 -- since color interpolation happends AFTER char changes,
                                     -- we compare colors with result of char interpolation
        colorDistance = fromMaybe 1 $ maximumMaybe l

        toString = map fst
        str1 = toString s1
        str2 = toString s2
        lPref = List.length $ commonPrefix str1 str2
        lSuff = List.length $ commonSuffix (drop lPref str1) (drop lPref str2)
        countTextChanges = max n1 n2 - (lPref + lSuff)
    in colorDistance + countTextChanges
-- | First interpolating characters, then color.
instance DiscreteInterpolation ColorString where
  interpolate c1 c2 i =
    let c2' = destructure c2
        (c1', remaining) = interpolateChars (destructure c1) c2' i
    in ColorString $ map (\(char,color) -> (Text.pack [char], color)) $
        if remaining >= 0
          then
            c1'
          else
            interpolateColors c1' c2' (negate remaining)

-- Restores white on black color in the end, and doesn't assume any
-- particular console color in the beginning.
safeBuildTxt :: ColorString -> Text
safeBuildTxt colorStr =
  let (builder,color) = buildTxt Nothing colorStr
  in toStrict $ toLazyText $ builder <> Builder.fromString (colorChange color whiteOnBlack)

-- | Converts to 'Data.Text.Lazy.Builder' with a minimum amount of foreground and background
-- color change control characters.
buildTxt :: Maybe LayeredColor
         -- ^ When this is a 'Just', assume that if no change control character is emitted,
         -- the string will be rendered in this color. It is used to spare a color change
         -- when possible.
         -> ColorString
         -- ^ the string to convert
         -> (Builder, Maybe LayeredColor)
         -- ^ Returns the converted string and the current color, if any.
buildTxt prev (ColorString l') =
  go l' prev mempty
 where
  go [] c b = (b, c)
  go ((txt,color):rest) c b =
    go rest (Just color) $
      b <>
      Builder.fromString (colorChange c color) <>
      Builder.fromText txt

replaceBackground :: Color8 Background -> ColorString -> ColorString
replaceBackground bg (ColorString l) =
  ColorString $ map (\(t, LayeredColor _ fg) -> (t, LayeredColor bg fg)) l


-- | Maps a 'ColorString' to a list of 'Char' and 'LayeredColor'.
destructure :: ColorString -> [(Char, LayeredColor)]
destructure (ColorString s) =
  go s
 where
  go [] = []
  go ((txt, color):ls) =
    map
      (flip (,) color)
      (Text.unpack txt)
     ++ go ls

-- | Reconstructs a minimal version of the string, i.e. keeps track of the current color
-- so that no 2 adjacent segments have the same color.
restructure :: [(Char, LayeredColor)] -> ColorString
restructure ls =
  ColorString $ reverse $ go (reverse ls) Nothing
 where
  go []           Nothing                 = []
  go []           (Just ([], _))          = []
  go []           (Just (l@(_:_), color)) = [(Text.pack l, color)]
  go ((c,col):xs) Nothing                 = go xs $ Just ([c],col)
  go ((c,col):xs) (Just (l@(_:_), color))
    | col == color                        = go xs (Just (c:l,color))
    | otherwise    = (Text.pack l, color) : go xs (Just ([c],col))
  go ((_, _):_)   (Just ([], _))          = error "logic"

colored' :: Text -> LayeredColor -> ColorString
colored' t c = ColorString [(t, c)]

colored :: Text -> Color8 Foreground -> ColorString
colored t c = colored' t $ onBlack c

-- | Counts the chars in the 'ColorString'
countChars :: ColorString -> Int -- TODO rename as length
countChars (ColorString cs) =
  sum $ map (Text.length . fst) cs

putStrLn :: ColorString -> IO ()
putStrLn = Text.putStrLn . safeBuildTxt

putStr :: ColorString -> IO ()
putStr = Text.putStr . safeBuildTxt

colorAttribute :: LayeredColor -> H.AttributeValue
colorAttribute (LayeredColor bg fg) = H.textValue $
  colorToHtml (color8ToUnitRGB fg) <>
  "background-" <>
  colorToHtml (color8ToUnitRGB bg)
