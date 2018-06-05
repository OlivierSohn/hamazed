{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Music.CTypes
      ( AHDSR(..)
      , Interpolation(..), itpToInt, allInterpolations
      , Ease(..)
      , EasedInterpolation(..)
      ) where

import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))
import           Control.DeepSeq as Exported(NFData(..))
import           Data.Binary(Binary(..))
import           Data.Set(Set)
import qualified Data.Set as Set

{-
The AHDSR envelope is like an ADSR envelope, except we allow to hold the value after the attack:

@
   | a |h| d |           |r|
       ___                                      < 1
      .    .
     .       _____________                      < s
    .                     .
 ___                       ___________________  < 0
   ^                     ^
   |                     |
   key is pressed        key is released

   Irrespective of wether the key is released when the note reached sustain or not,
    the envelope will touch 0 in 'release' samples.
@
-}
data AHDSR = AHDSR {
    ahdsrAttack,ahdsrHold,ahdsrDecay,ahdsrRelease :: {-# UNPACK #-} !Int
  , ahdsrAttackItp,ahdsrDecayItp,ahdsrReleaseItp :: !Interpolation
  , ahdsrSustain :: {-# UNPACK #-} !Float
} deriving(Generic, Show, Eq, Data, Ord)
instance NFData AHDSR
instance Binary AHDSR

data Ease =
    EaseIn
  | EaseOut
  | EaseInOut
  deriving(Generic, Ord, Eq, Data)
instance Binary Ease
instance NFData Ease

instance Show Ease where
  show = \case
    EaseIn -> "easeIn"
    EaseOut -> "easeOut"
    EaseInOut -> "easeInOut"

allEases :: [Ease]
allEases = [EaseIn, EaseOut, EaseInOut]

-- cf. enum interpolation in interpolation.h
easeBaseIdx :: Ease -> Int
easeBaseIdx = \case
  EaseIn -> 3
  EaseOut -> 12
  EaseInOut -> 21

data Interpolation =
    Linear
  | ProportionaValueDerivative
  | Eased !Ease !EasedInterpolation
  deriving(Generic, Ord, Eq, Data)
instance Binary Interpolation
instance NFData Interpolation

instance Show Interpolation where
  show = \case
    Linear -> "Linear"
    ProportionaValueDerivative -> "PVD"
    Eased e i -> unwords [show e, show i]

allInterpolations :: Set Interpolation
allInterpolations = Set.fromList $
  [Linear, ProportionaValueDerivative] ++
  [Eased e i | e <- allEases, i <- allEasedInterpolations]

-- cf. enum interpolation in interpolation.h
itpToInt :: Interpolation -> Int
itpToInt = \case
  Linear -> 0
  ProportionaValueDerivative -> 1
  Eased e i -> easeBaseIdx e + interpolationIdx i

data EasedInterpolation =
    Ord2
  | Ord3
  | Ord4
  | Ord5
  | Sine
  | Exp
  | Circ
  deriving(Generic, Ord, Eq, Data)
instance Binary EasedInterpolation
instance NFData EasedInterpolation

instance Show EasedInterpolation where
  show = \case
    Ord2 -> "x^2"
    Ord3 -> "x^3"
    Ord4 -> "x^4"
    Ord5 -> "x^5"
    Sine -> "sin"
    Exp -> "exp"
    Circ -> "circ"

allEasedInterpolations :: [EasedInterpolation]
allEasedInterpolations = [Ord2, Ord3, Ord4, Ord5, Sine, Exp, Circ]

-- cf. enum interpolation in interpolation.h
interpolationIdx :: EasedInterpolation -> Int
interpolationIdx = \case
  Ord2 -> 0
  Ord3 -> 1
  Ord4 -> 2
  Ord5 -> 3
  Sine -> 4
  Exp -> 5
  Circ -> 6
