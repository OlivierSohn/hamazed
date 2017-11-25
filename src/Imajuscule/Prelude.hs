{-# LANGUAGE NoImplicitPrelude #-}

-- initially I needed this custom Prelude to hide putStr and putChar,
-- since I provide equivalent functions that should be used instead to
-- render the game

module Imajuscule.Prelude (
                            -- | Prelude reexports
                            Eq
                          , Show
                          , Num
                          , Enum
                          , Ord
                          , Bool(..)
                          , Char
                          , Float
                          , IO
                          , Int
                          , Maybe(..)
                          , Either(..)
                          , Ordering(..)
                          , all
                          , any
                          , notElem
                          , null
                          , minimum
                          , either
                          , maybe
                          , zipWith
                          , map
                          , concatMap
                          , filter
                          , replicate
                          , show
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
                          , (.)
                          , (++)
                          , (=<<)
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
                          , (+)
                          , (-)
                          , (/)
                          , (^)
                          , pred
                          , succ
                          , realToFrac
                          , fromIntegral
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
                          -- | Data.Monoid reexports
                          , (<>)
                          , undefined
                          -- | Control.Exception reexports
                          , assert
                          -- | Data.Word reexports
                          , Word8
                          -- | Data.Text reexports
                          , Text
                          ) where

import           Prelude

import           Control.Applicative( (<|>) )
import           Control.Monad( when, void )
import           Control.Exception( assert )

import           Data.Monoid ((<>))
import           Data.Word( Word8 )
import           Data.Text( Text )
