{-# LANGUAGE NoImplicitPrelude #-}

-- initially I needed this custom Prelude to hide putStr and putChar,
-- since I provide equivalent functions that should be used instead to
-- render the game

module Imajuscule.Prelude (
                            -- | Prelude reexports
                            Eq
                          , Show
                          , Num
                          , Ord
                          , Bool(..)
                          , Char
                          , Float
                          , IO
                          , Int
                          , Maybe(..)
                          , Either(..)
                          , Ordering(..)
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
                          , otherwise
                          , id
                          , Monad(..)
                          -- | Control.Applicative reexports
                          , (<|>)
                          -- | Control.Monad reexports
                          , when
                          -- | Data.Monoid reexports
                          , (<>)
                          , undefined
                          ) where

import           Prelude

import           Control.Applicative( (<|>) )
import           Control.Monad( when )

import           Data.Monoid ((<>))
