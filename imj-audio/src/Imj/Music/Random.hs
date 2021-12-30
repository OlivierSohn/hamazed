
module Imj.Music.Random(
          pickRandom,
          pickRandomInstructions,
          pickRandomWeighted,
          pickRandomInstructionsWeighted)
  where

import           System.Random.MWC(uniform, Variate(..), GenIO)

import Data.List(foldl')

import           Imj.Prelude
import           Imj.Music.Instruction(Instruction(..))

pickRandom :: GenIO -> Int -> [a] -> IO [a]
pickRandom rng count l = do
  ints <- replicateM count $ uniform rng :: IO [Int]
  return $ map (((!!) l) . idx) ints
 where
  idx i = mod i m
  m = length l

pickRandomWeighted :: GenIO -> Int -> [(a, Float)] -> IO [a]
pickRandomWeighted rng count l = do
  replicateM count pick
 where
  pick = do
    word <- uniform rng :: IO Word
    return $ accWeightToValue 0 l $ intToAccWeight word
  intToAccWeight i = sumWeights * ((fromIntegral i) / (fromIntegral (maxBound :: Word)))
  sumWeights = foldl' (\acc (_, w) -> acc + w) 0 l
  accWeightToValue _ [] _ = error "not found"
  accWeightToValue accWeight ((val, weight):es) aw =
    if (accWeight + weight >= aw)
      then val
      else accWeightToValue (accWeight + weight) es aw

{--
  Ensures Instructions are coherent, i.e
 - we use Extend only if a note is being played
--}
sanitizeInstructions :: [Instruction] -> [Instruction]
sanitizeInstructions insns = sanitize Nothing [] insns
 where
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
            Rest -> Rest
            _ -> Extend)
          _ -> r)
        mayPrev): acc)
      nextRemain

pickRandomInstructions :: GenIO -> Int -> [Instruction] -> IO [Instruction]
pickRandomInstructions rng count l = do
  insns <- pickRandom rng count l
  return $ sanitizeInstructions insns

pickRandomInstructionsWeighted :: GenIO -> Int -> [(Instruction, Float)] -> IO [Instruction]
pickRandomInstructionsWeighted rng count l = do
  insns <- pickRandomWeighted rng count l
  return $ sanitizeInstructions insns
