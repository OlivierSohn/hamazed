
module Imj.Music.Random(pickRandom, pickRandomInstructions)
  where

import           System.Random.MWC(create, uniform, Variate(..))

import           Imj.Prelude
import           Imj.Music.Instruction(Instruction(..))

pickRandom :: Int -> [a] -> IO [a]
pickRandom count l = do
  g <- create
  ints <- replicateM count $ uniform g :: IO [Int]
  return $ map (((!!) l) . idx) ints
 where
  idx i = mod i m
  m = length l

{--
  Ensures Instructions are coherent, i.e
 - we use Extend only if a note is being played
--}
pickRandomInstructions :: Int -> [Instruction] -> IO [Instruction]
pickRandomInstructions count l = do
  g <- create
  ints <- replicateM count $ uniform g :: IO [Int]
  return $ sanitize Nothing [] $ map (((!!) l) . idx) ints
 where
  idx i = mod i m
  m = length l
  sanitize mayPrev acc remain = case remain of
    [] -> reverse acc
    (r:nextRemain) ->
      sanitize
      (Just r)
      ((maybe
        (case r of
          Extend -> Rest
          _ -> r)
        (\prev -> case r of
          Extend -> (case prev of
            Note _ _ -> Extend
            _ -> Rest)
          _ -> r)
        mayPrev): acc)
      nextRemain
