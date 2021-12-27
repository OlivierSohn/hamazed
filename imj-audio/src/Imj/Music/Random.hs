
module Imj.Music.Random(pickRandom)
  where

import           System.Random.MWC(create, uniform)

import           Imj.Prelude
import           Imj.Music.Instruction(Instruction(..))

{--
  Ensures Instructions are coherent, i.e
 - we use Extend only if a note is being played
--}
pickRandom :: Int -> [Instruction] -> IO [Instruction]
pickRandom count l = do
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
