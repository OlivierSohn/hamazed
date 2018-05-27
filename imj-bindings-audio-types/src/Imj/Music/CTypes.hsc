{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Imj.Music.CTypes
      ( AHDSR(..)
      ) where

import           Data.Data(Data(..))
import           GHC.Generics(Generic(..))
import           Control.DeepSeq as Exported(NFData(..))
import           Data.Binary(Binary(..))
import           Foreign.Storable(Storable(..))

#include "public_c.h"

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
  , ahdsrSustain :: {-# UNPACK #-} !Float
} deriving(Generic, Show, Eq, Data, Ord)
instance NFData AHDSR
instance Binary AHDSR

instance Storable AHDSR where
  alignment _ = #{alignment AHDSR_t}
  sizeOf _    = #{size      AHDSR_t}

  peek p      = do
    a <- #{peek AHDSR_t, attack}   p
    h <- #{peek AHDSR_t, hold}     p
    d <- #{peek AHDSR_t, decay}    p
    r <- #{peek AHDSR_t, release}  p
    s <- #{peek AHDSR_t, sustain}  p
    return $ AHDSR a h d r s

  poke p AHDSR{..} = do
    #{poke AHDSR_t, attack}  p ahdsrAttack
    #{poke AHDSR_t, hold}    p ahdsrHold
    #{poke AHDSR_t, decay}   p ahdsrDecay
    #{poke AHDSR_t, release} p ahdsrRelease
    #{poke AHDSR_t, sustain} p ahdsrSustain
