{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Audio.Envelope
      ( -- * Types
        AHDSR'Envelope(..)
      , ReleaseMode(..)
      , Interpolation(..)
      , Ease(..)
      , EasedInterpolation(..)
      -- * Analyze envelopes
      , analyzeAHDSREnvelope
      -- * Utilities
      , cycleReleaseMode
      , interpolationToCInt, allInterpolations
      ) where

import           Control.DeepSeq as Exported(NFData(..))
import           Data.Binary
import           Data.Data(Data(..))
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Vector.Unboxed(Vector, unsafeFreeze)
import           Data.Vector.Unboxed.Mutable(new, unsafeWrite)
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics(Generic(..))

import           Imj.Data.AlmostFloat


data ReleaseMode =
    KeyRelease
    -- ^ The envelope release is triggered by 'StopNote'
  | AutoRelease
  -- ^ 'StopNote' is not taken into account : the envelope release immediately
  -- follows the envelope decay (the envelope sustain phase is skipped).
  deriving(Generic, Ord, Data, Eq, Show)
instance Enum ReleaseMode where
  fromEnum = \case
    KeyRelease -> 0
    AutoRelease -> 1
  toEnum = \case
    0 -> KeyRelease
    1 -> AutoRelease
    n -> error $ "out of range:" ++ show n
instance NFData ReleaseMode
instance Binary ReleaseMode

-- | Returns the next value of 'ReleaseMode'
cycleReleaseMode :: ReleaseMode -> ReleaseMode
cycleReleaseMode AutoRelease = KeyRelease
cycleReleaseMode e = succ e

-- | Returns lists of consecutive envelope values.
--
-- If the 'ReleaseMode' is 'AutoRelease',
-- a single list is returned, covering all envelope phases, from attack to release.
--
-- If the 'ReleaseMode' is 'KeyRelease', two lists are returned:
--
-- * The first list covers phases from attack to the beginning of sustain.
-- * The second list covers the end of sustain to the release phase.

analyzeAHDSREnvelope :: ReleaseMode
                     -> AHDSR'Envelope
                     -> IO [Vector Double]
analyzeAHDSREnvelope e (AHDSR'Envelope a h d r ai di ri s) =
  alloca $ \ptrNElems -> alloca $ \ptrSplitAt -> do
    buf <- analyzeAHDSREnvelope_ (fromIntegral $ fromEnum e) (fromIntegral a) (interpolationToCInt ai) (fromIntegral h) (fromIntegral d) (interpolationToCInt di) (realToFrac s) (fromIntegral r) (interpolationToCInt ri) ptrNElems ptrSplitAt
    nElems <- fromIntegral <$> peek ptrNElems
    split <- fromIntegral <$> peek ptrSplitAt
    let slices =
          if split < 0
            then
              [(0,nElems-1)
              ]
            else
              [(0,split-1)
              ,(split,nElems-1)
              ]
    res <- mapM (uncurry $ takeBuffer buf) slices
    imj_c_free buf
    return res
   where
    takeBuffer buf iStart iEnd = do
      uv <- new (1 + iEnd - iStart)
      mapM_
        (\i -> do
          val <- (peek $ plusPtr buf $ i * (sizeOf (undefined :: CDouble))) :: IO CDouble
          unsafeWrite uv (i-iStart) $ realToFrac val)
        [iStart..iEnd]
      unsafeFreeze uv

foreign import ccall "analyzeAHDSREnvelope_"
  analyzeAHDSREnvelope_ :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr CDouble)

-- https://stackoverflow.com/questions/43372363/releasing-memory-allocated-by-c-runtime-from-haskell
foreign import ccall "imj_c_free" imj_c_free :: Ptr a -> IO ()



{- |
The AHDSR envelope is like an <https://www.wikiaudio.org/adsr-envelope/ ADSR envelope>
, except that the signal can be hold after the attack. Plotting the graph of values returned
by 'analyzeAHDSREnvelope' gives:

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
@

'ahdsrAttack', 'ahdsrDecay' and 'ahdsrRelease' durations are hints from which the
audio engine will deduce the actual attack / decay / release durations:
actual values will be at least the hint values, and at least 2.5 times
the period of the sound, to avoid any audible crack.

Attack, Decay and Release phases can be shaped using an 'Interpolation'.
-}
data AHDSR'Envelope = AHDSR'Envelope {
    ahdsrAttack :: {-# UNPACK #-} !Int
    -- ^ Hint for the number of samples between when the key is pressed and when the
    -- envelope reaches 1
  , ahdsrHold :: {-# UNPACK #-} !Int
    -- ^ Hint for the number of samples during which the full value, 1, will be maintained
    -- after the attack.
  , ahdsrDecay :: {-# UNPACK #-} !Int
    -- ^ Hint for the number of samples between the end of the Hold phase and the beginning of
    -- sustain.
  , ahdsrRelease :: {-# UNPACK #-} !Int
    -- ^ Release duration:  hint for number of samples between the moment the key is released and
    -- the moment the envelope reaches 0 (irrespective of wether the envelope had enough time
    -- to reach sustain or not).
  , ahdsrAttackItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the attack.
  , ahdsrDecayItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the decay.
  , ahdsrReleaseItp :: !Interpolation
  -- ^ Determines how the envelope will be shaped during the release.
  , ahdsrSustain :: {-# UNPACK #-} !AlmostFloat
  -- ^ The sustain value. Must be in the [0,1] range
} deriving(Generic, Show, Eq, Data, Ord)
instance NFData AHDSR'Envelope
instance Binary AHDSR'Envelope

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
easeBaseIdx :: Ease -> CInt
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

-- | The 'Set' of possible 'Interpolation' values.
allInterpolations :: Set Interpolation
allInterpolations = Set.fromList $
  [Linear, ProportionaValueDerivative] ++
  [Eased e i | e <- allEases, i <- allEasedInterpolations]

-- | Returns the corresponding value of the C-enum interpolation defined in interpolation.h
interpolationToCInt :: Interpolation -> CInt
interpolationToCInt = \case
  Linear -> 0
  ProportionaValueDerivative -> 1
  Eased e i -> easeBaseIdx e + interpolationIdx i

data EasedInterpolation =
    Ord2
    -- ^ Quadratic easing.
  | Ord3
    -- ^ Cubic easing.
  | Ord4
    -- ^ Quartic easing.
  | Ord5
    -- ^ Quintic easing.
  | Sine
    -- ^ Sine easing.
  | Exp
    -- ^ Exponential easing.
  | Circ
    -- ^ Cirular easing (using square root).
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
interpolationIdx :: EasedInterpolation -> CInt
interpolationIdx = \case
  Ord2 -> 0
  Ord3 -> 1
  Ord4 -> 2
  Ord5 -> 3
  Sine -> 4
  Exp -> 5
  Circ -> 6
