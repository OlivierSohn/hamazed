
module Imj.Music.Random(
          pickRandom,
          pickRandomInstructions,
          pickRandomWeighted,
          pickRandomInstructionsWeighted)
  where

import           System.Random.MWC(create, uniform, Variate(..))

import Data.List(foldl')

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

pickRandomWeighted :: Int -> [(a, Float)] -> IO [a]
pickRandomWeighted count l = do
  g <- create
  replicateM count $ pick g
 where
  pick g = do
    word <- uniform g :: IO Word
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
            Note _ _ -> Extend
            _ -> Rest)
          _ -> r)
        mayPrev): acc)
      nextRemain

pickRandomInstructions :: Int -> [Instruction] -> IO [Instruction]
pickRandomInstructions count l = do
  insns <- pickRandom count l
  return $ sanitizeInstructions insns

pickRandomInstructionsWeighted :: Int -> [(Instruction, Float)] -> IO [Instruction]
pickRandomInstructionsWeighted count l = do
  insns <- pickRandomWeighted count l
  return $ sanitizeInstructions insns
