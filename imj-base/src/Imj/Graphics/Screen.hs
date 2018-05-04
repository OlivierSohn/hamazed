{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Graphics.Screen
        ( Screen(..)
        , mkScreen
        ) where

import           Imj.Prelude
import           Imj.Geo.Discrete.Types

data Screen = Screen {
    _screenSize :: {-unpack sum-} !(Maybe Size)
  -- ^ Maybe we couldn't get the screen size.
  , _screenCenter :: {-# UNPACK #-} !(Coords Pos)
  -- ^ The center is deduced from screen size, if any, or guessed.
}

mkScreen :: Maybe Size -> Screen
mkScreen sz =
  let center = maybe
                (Coords 40 80)
                (\(Size h w) -> Coords (fromIntegral $ quot h 2)
                                       (fromIntegral $ quot w 2))
                  sz
  in Screen sz center
