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

{- |
The AHDSR envelope is like an ADSR envelope, except that the signal can be hold after the attack:

@
   | a |h| d |           |r|
       ---                                      < 1
      .    .
     .       -------------                      < s
    .                     .
 ---                       -------------------  < 0
   ^                     ^
   |                     |
   key is pressed        key is released

   Attack, decay and release phases can be shaped according to 'Interpolation'.
@
-}
data AHDSR = AHDSR {
    ahdsrAttack :: {-# UNPACK #-} !Int
    -- ^ Attack duration : the number of samples between when the key is pressed and when the
    -- envelope reaches 1
  , ahdsrHold :: {-# UNPACK #-} !Int
    -- ^ Hold duration: the number of samples during which the full value, 1, will be maintained
    -- after the attack.
  , ahdsrDecay :: {-# UNPACK #-} !Int
    -- ^ Decay duration: the number of samples between the end of the Hold phase and the beginning of
    -- sustain.
  , ahdsrRelease :: {-# UNPACK #-} !Int
    -- ^ Release duration: the number of samples between the moment the key is released and
    -- the moment the envelope reaches 0 (irrespective of wether the envelope had enough time
    -- to reach sustain or not).
  , ahdsrAttackItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the attack.
  , ahdsrDecayItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the decay.
  , ahdsrReleaseItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the release.
  , ahdsrSustain :: {-# UNPACK #-} !Float
  -- ^ The sustain value. Must be in the [0,1] range
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
