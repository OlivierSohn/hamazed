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
                          , String
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
                          , length
                          , take
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
                          ) where

import Prelude

import           Control.Applicative( (<|>) )
import           Control.Monad( when )
