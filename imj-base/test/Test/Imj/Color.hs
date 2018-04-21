{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Color
         ( testColor
         ) where

import           Imj.Prelude
import           Prelude(length, concat)

import           Data.Set(fromList)

import           Imj.Graphics.Color.Types

import           Imj.Graphics.Color.Hue
import           Imj.Graphics.Color.Hue.Internal

import           Imj.Data.AlmostFloat

testColor :: IO ()
testColor = do
  testHue
  testRotateColorsWithoutHue
  testRotateColorsWithHue
  testIntensities

testIntensities :: IO ()
testIntensities = do
  mapM_ (\c -> countHuesOfSameIntensity c `shouldBe` 1) colorsWithoutHues
  mapM_ (\c -> sameIntensityHues c `shouldBe` [c]) colorsWithoutHues
  mapM_
    (\i -> do
      let ref = saturatedCycle $ mkIntensity i
          n = length ref
      case ref of
        [] -> error "test"
        (r:_) -> do
          n `shouldBe` countHuesOfSameIntensity r
          fromList ref `shouldBe` fromList (sameIntensityHues r))
    [1..5]

testRotateColorsWithHue :: IO ()
testRotateColorsWithHue = do
  rotateHue (1/3)  (rgb 1 0 0) `shouldBe` rgb 0 1 0
  rotateHue (2/3)  (rgb 1 0 0) `shouldBe` rgb 0 0 1
  rotateHue (-2/3) (rgb 1 0 0) `shouldBe` rgb 0 1 0
  rotateHue (-1/3) (rgb 1 0 0) `shouldBe` rgb 0 0 1
  rotateHue (1/3)  (rgb 0 1 0) `shouldBe` rgb 0 0 1
  rotateHue (2/3)  (rgb 0 1 0) `shouldBe` rgb 1 0 0
  rotateHue (-2/3) (rgb 0 1 0) `shouldBe` rgb 0 0 1
  rotateHue (-1/3) (rgb 0 1 0) `shouldBe` rgb 1 0 0

  rotateHue (1/3)  (rgb 4 3 3) `shouldBe` rgb 3 4 3

testRotateColorsWithoutHue :: IO ()
testRotateColorsWithoutHue =
  mapM_
    (\(c, dh) -> rotateHue dh c `shouldBe` c)
    [(c, dh) | c <- colorsWithoutHues
             , dh <- someHues]
 where
  someHues = [0, 0.2, -0.2, 1, -1, 3.5, -3.5]

testHue :: IO ()
testHue = do
  mapM_
    (\c -> hue c `shouldBe` Nothing)
    colorsWithoutHues

  -- test well-known hues
  testSaturatedColorHue (\x -> rgb x 0 0) (0/6)
  testSaturatedColorHue (\x -> rgb x x 0) (1/6)
  testSaturatedColorHue (\x -> rgb 0 x 0) (2/6)
  testSaturatedColorHue (\x -> rgb 0 x x) (3/6)
  testSaturatedColorHue (\x -> rgb 0 0 x) (4/6)
  testSaturatedColorHue (\x -> rgb x 0 x) (5/6)

  -- verify that removing the gray component doesn't change the hue.
  hue (rgb 1 2 3) `shouldBe` hue (rgb 0 1 2)

  -- verify that for every saturated cycle, when walking the cycle by increasing hues,
  -- the hue increment between two adjacent elements is constant.
  mapM_
    (verifyIncrementIsConstant . map (fromMaybe (error "unexpected") . hue) . saturatedCycle . mkIntensity)
    [1..5]
 where
  testSaturatedColorHue makeColor hueValue =
    mapM_ (\x -> fmap almost (hue (makeColor x)) `shouldBe` Just (almost hueValue)) [1..5]
  verifyIncrementIsConstant l' = go l' Nothing
   where
    go []  _ = return ()
    go [_] _ = return ()
    go (x:rest@(y:_)) ref =
      maybe
        (go rest $ Just thisIncrement)
        (\refIncrement -> do
          refIncrement `shouldBe` thisIncrement
          go rest ref
          )
        ref
     where
      thisIncrement = almost $ y - x


-- grays, blacks and whites have no hue
colorsWithoutHues :: [Color8 a]
colorsWithoutHues = concat
  [ map (\x -> rgb x x x) [0..5]
  , map gray [0..23]
  ]

-- returns a list of colors representing a saturated cycle of a given intensity.
-- The cycle starts at pure red (0 hue) and is built in order to have strictly increasing hues.
saturatedCycle :: Intensity -> [Color8 a]
saturatedCycle (Intensity i) = concat
  [ map (\x -> rgb i     x     0    ) [0..pred i]
  , map (\x -> rgb (i-x) i     0    ) [0..pred i]
  , map (\x -> rgb 0     i     x    ) [0..pred i]
  , map (\x -> rgb 0     (i-x) i    ) [0..pred i]
  , map (\x -> rgb x     0     i    ) [0..pred i]
  , map (\x -> rgb i     0     (i-x)) [0..pred i]
  ]

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
