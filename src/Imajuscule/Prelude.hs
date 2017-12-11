{-# LANGUAGE NoImplicitPrelude #-}

-- initially I needed this custom Prelude to hide putStr and putChar,
-- since I provide equivalent functions that should be used instead to
-- render the game

module Imajuscule.Prelude (
                            -- | Prelude reexports
                            Eq
                          , Show(..)
                          , Num
                          , Enum
                          , Integral
                          , Ord
                          , Monoid(..)
                          , Bool(..)
                          , Char
                          , Float
                          , IO
                          , Int
                          , Maybe(..)
                          , Either(..)
                          , Ordering(..)
                          , fmap
                          , all
                          , any
                          , notElem
                          , null
                          , minimum
                          , maximum
                          , either
                          , maybe
                          , zipWith
                          , map
                          , concatMap
                          , filter
                          , replicate
                          , mapM
                          , mapM_
                          , take
                          , tail
                          , last
                          , head
                          , drop
                          , iterate
                          , unwords
                          , flip
                          , const
                          , sum
                          , zip
                          , fst
                          , snd
                          , compare
                          , not
                          , or
                          , (.)
                          , (++)
                          , (=<<)
                          , (<=<)
                          , (<$>)
                          , (==)
                          , (/=)
                          , (>)
                          , (<)
                          , (>=)
                          , (<=)
                          , (||)
                          , (&&)
                          , ($)
                          , (*)
                          , (**)
                          , (+)
                          , (-)
                          , (/)
                          , (^)
                          , pred
                          , succ
                          , realToFrac
                          , fromIntegral
                          , fromRational
                          , recip
                          , signum
                          , sin
                          , cos
                          , mod
                          , min
                          , max
                          , abs
                          , negate
                          , div
                          , quot
                          , even
                          , odd
                          , error
                          , pi
                          , floor
                          , ceiling
                          , otherwise
                          , id
                          , curry
                          , uncurry
                          , maxBound
                          , Monad(..)
                          -- | Control.Applicative reexports
                          , (<|>)
                          -- | Control.Monad reexports
                          , when
                          , void
                          -- | Control.Monad.IO.Class reexports
                          , liftIO
                          -- | Data.Monoid reexports
                          , (<>)
                          , undefined
                          -- | Control.Exception reexports
                          , assert
                          -- | Data.Word reexports
                          , Word8
                          -- | Data.Text reexports
                          , Text
                          -- | Data.Ratio reexports
                          , (%)
                          ) where

import           Prelude

import           Control.Applicative( (<|>) )
import           Control.Monad( when, void, (<=<), Monad )
import           Control.Monad.IO.Class( liftIO )
import           Control.Exception( assert )
import           Data.Monoid ((<>))
import           Data.Word( Word8 )
import           Data.Text( Text )
import           Data.Ratio((%))
