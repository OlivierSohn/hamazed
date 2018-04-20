{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Topology
          ( testTopology
          ) where

import           Imj.Prelude
import           Prelude(print, putStrLn, length)
import           Data.Either(rights)
import           Data.List(foldl', take, concat)
import qualified Data.List.NonEmpty as NE(toList)
import           Data.List.NonEmpty(NonEmpty(..))
import           Data.Primitive.ByteArray
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import           System.Random.MWC(GenIO, create)

import           Imj.Data.AlmostFloat
import qualified Imj.Data.Matrix.Cyclic as Cyclic
import qualified Imj.Data.Matrix.Unboxed as Unboxed

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Graphics.Text.Render
import           Imj.Util

testTopology :: IO ()
testTopology = do
  testAirOnEveryFronteer
  testNumComps
  testComponentsSizesWellDistributed
  testComponentsNearby
  testMinCountAirBlocks

forAnyNumberOfComponents :: (ComponentCount -> IO ())
                         -> IO ()
forAnyNumberOfComponents = forM_ [0..10]

testAirOnEveryFronteer :: IO ()
testAirOnEveryFronteer = forAnyNumberOfComponents $ \n -> do
  print n
  putStrLn "WithoutAir"
  debugForM_ matricesWithoutAirOnEveryFronteer
    (const . (`shouldBe` Left UnusedFronteers) . uncurry (matchTopology NCompsNotRequired n))
  putStrLn "WithtAir"
  debugForM_ matricesWithAirOnEveryFronteer
    (const . (`shouldNotBe` Left UnusedFronteers) . uncurry (matchTopology NCompsNotRequired n))
  putStrLn "WithoutAir 2"
  debugForM_ matricesWithoutAirOnEveryFronteer
    (\m expected -> uncurry (matchTopology (NCompsRequiredWithMargin 100) n) m `shouldBe` Left (CC UnusedFronteers' expected))
  putStrLn "WithtAir 2"
  debugForM_ matricesWithAirOnEveryFronteer
    (\m expected -> uncurry (matchTopology (NCompsRequiredWithMargin 100) n) m `shouldNotBe` Left (CC UnusedFronteers' expected))


mkKeys :: Unboxed.Matrix Material -> IO (ByteArray,SmallMatInfo)
mkKeys m = do
  ba <- newAlignedPinnedByteArray (ceilToMultiple 64 $ nBlocks * 8) 64 >>= unsafeFreezeByteArray
  return (ba,SmallMatInfo (fromIntegral nAir) $ Cyclic.fromList r c $ reverse l)
 where
  r = Unboxed.nrows m
  c = Unboxed.ncols m
  nBlocks = r*c
  (l, nAir) = foldl'
      (\(l',k) v -> case v of
        Wall -> (MaterialAndKey 0xFFFF:l',k)
        Air -> (MaterialAndKey k:l', k+1))
      ([],0)
      $ Unboxed.toList m

debugForM_ :: [(a,Unboxed.Matrix Material)]
           -> ((ByteArray,SmallMatInfo) -> a -> IO ())
           -> IO ()
debugForM_ l act = forM_' l (\(n,e@(_,mat)) -> do
  mapM_ putStrLn $ showInBox' $ writeWorld mat
  act e n)

forM_' :: [(a,Unboxed.Matrix Material)] -> ((a,(ByteArray,SmallMatInfo)) -> IO ()) -> IO ()
forM_' l act = forM l (\(a,b) -> (,) a <$> mkKeys b) >>= mapM_ act

displayNWorlds :: Int -> [SmallMatInfo] -> [String]
displayNWorlds lineLength = concat . reverse .
  foldl'
    (\(line:prevLines) m -> case line of
      [] -> m:prevLines
      x:_ ->
        if length x < lineLength
          then
            addRight line 1 m:prevLines
          else
            m:line:prevLines)
    [[]]
    . map (showInBox' . writeWorld)

testNumComps :: IO ()
testNumComps = forAnyNumberOfComponents $ \n -> do
  print n
  putStrLn "WithNComponents"
  forM_' matricesWithNComponents (\(expected, m) -> getComponentCount (uncurry (matchTopology (NCompsRequiredWithMargin 100) n) m) `shouldBe` Just expected)

getComponentCount :: TopoMatch -> Maybe ComponentCount
getComponentCount (Left (CC _ c)) = Just c
getComponentCount (Left _) = Nothing
getComponentCount (Right (SmallWorld _ topo)) = Just $ ComponentCount $ length $ getConnectedComponents topo

testComponentsSizesWellDistributed :: IO ()
testComponentsSizesWellDistributed = do
  putStrLn "WithNNotWellDistributedComponents"
  forM_' matricesWithNNotWellDistributedComponents
    (\(expected, m) -> uncurry (matchTopology (NCompsRequiredWithMargin 100) expected) m `shouldBe` Left (CC ComponentsSizesNotWellDistributed expected))
  putStrLn "WithNWellDistributedComponentsSpaceWellUsed"
  forM_' matricesWithNWellDistributedComponentsSpaceWellUsed
    (\(expected, m@(_,mat)) -> do
      mapM_ putStrLn $ showInBox' $ writeWorld mat
      either (fail "expected Right") (const $ return ()) $ uncurry (matchTopology (NCompsRequiredWithMargin 100) expected) m)

testComponentsNearby :: IO ()
testComponentsNearby = do
  putStrLn "WithNWellDistributedComponentsSpaceNotWellUsed"
  forM_' matricesWithNWellDistributedComponentsSpaceNotWellUsed
    (\(expected, m) -> uncurry (matchTopology (NCompsRequiredWithMargin 100) expected) m `shouldBe` Left (CC SpaceNotUsedWellEnough expected))

testMinCountAirBlocks :: IO ()
testMinCountAirBlocks = do
  gen <- create -- use a deterministic random numbers source
  forM_ nComps_expectedMinAir $ uncurry $ go gen
 where
  sz = Size 5 3
  countBlocks = area sz
  nComps_expectedMinAir =
    [ (0,Just 0)
    , (1,Just 7), (2,Just 6), (3,Just 6) -- for the (3,Just 6) case, in 'minCountAirBlocks' an adjustment was made
                                         -- to produce a well-distributed result.
    , (4,Just 4) , (5,Just 5), (6,Just 6), (7,Just 7), (8, Just 8), (9, Nothing)
    ]

  go :: GenIO -> ComponentCount -> Maybe Int -> IO ()
  go gen nComps expectedCountMinAir = do
    putStrLn $ "testMinCountAirBlocks " ++ show (nComps, expectedCountMinAir)
    minCountAirBlocks nComps sz `shouldBe` expectedCountMinAir
    when (nComps > 0) $ go' 0
   where
    go' n = maybe
      -- verify that it's not possible to generate a world of that size with that many components.
      (do
        let wallProba = 1 - fromIntegral nComps / fromIntegral countBlocks
        validWorlds <- rights <$> generateAtLeastN 10000 (NE.toList <$> generate wallProba)
        length validWorlds `shouldBe` 0)
      -- verify that the number of air blocks in worlds of that size with that count of components
      -- is according to what is expected, and that it is possible to reach the minimum.
      (\minAirCount -> do
        let wallAirRatio = 1 - fromIntegral minAirCount / fromIntegral countBlocks :: AlmostFloat
        successes <- generateAtLeastN nSuccesses $ do
          smallMats <- map getSmallMatrix . rights . NE.toList <$> generate wallAirRatio
          putStrLn $ "generated " ++ show (length smallMats)
          forM_ smallMats (verifyMat "generated")
          return smallMats
        nAirElts <- mapM countAirElements successes
        let z = zip nAirElts successes
            minimals = filter (\(countAirElems, _) -> countAirElems == minAirCount) z
            totalSuccesses = n + length successes
        mapM_ (`shouldBeBiggerOrEqualTo` minAirCount) $ map fst z
        bool
          (do
            let lenMinimals = length minimals
            putStrLn $
              "Found " ++
              show lenMinimals ++
              " minimal configuration(s) among " ++
              show totalSuccesses ++
              " successes" ++ bool ":" " (showing 20 first only):" (lenMinimals > 20)

            mapM_ putStrLn $ displayNWorlds 100 $ take 20 $ map snd minimals
            )
          (go' totalSuccesses)
          $ null minimals )
      expectedCountMinAir

    nSuccesses = 100 -- I verified the test passes also with 10000 but it is a lot slower.

    -- we use 'mkSmallMatUnchecked' because we want to test the lower bounds.
    -- with 'mkSmallMat', the test is not relevant.
    generate proba = do
      (ba,m) <- mkSmallMatUnchecked gen proba sz
      verifyMat "generate" m
      let res = (:|[]) $ matchTopology' NCompsNotRequired nComps m
--          res = matchAndVariate nComps
      --     (
          --Just $ Variants
            --(pure $ Rotate $ RotationDetail Cyclic.Order1 (ComponentCount 5))
  --          Nothing
    --        )
        --    ba m
          smallMats = map getSmallMatrix $ rights $ NE.toList res
      mapM_ (verifyMat "afterMatch") smallMats
      return res

verifyMat :: String -> SmallMatInfo -> IO ()
verifyMat s sm@(SmallMatInfo nAir m) = do
  let fs = do
            mapM_ putStrLn $ showInBox' $ writeWorld sm
            print m
  verifyAirKeys fs s (Cyclic.mvect m) nAir

mkSmallMatUnchecked :: GenIO
                    -> AlmostFloat
                    -- ^ Probability to generate a wall
                    -> Size
                    -- ^ Size of the matrix
                    -> IO (ByteArray,SmallMatInfo)
mkSmallMatUnchecked gen wallAirRatio s@(Size nRows nCols) = do
  let nBlocks = area s
  graphArray <- newAlignedPinnedByteArray (ceilToMultiple 64 $ nBlocks * 8) 64 >>= unsafeFreezeByteArray
  v <- MS.unsafeNew nBlocks
  nAir <- fillSmallVector gen wallAirRatio v

  v' <- S.unsafeFreeze v
  let !mat = SmallMatInfo (fromIntegral nAir) $ Cyclic.fromVector (fromIntegral nRows) (fromIntegral nCols) v'
  a <- verifyMat "mkSmallMatUnchecked" mat
  return $ a `deepseq` (graphArray, mat)

verifyAirKeys :: (Integral a) => IO () -> String -> S.Vector MaterialAndKey -> a -> IO ()
verifyAirKeys onError name v nAir = do
  -- verify that air keys are [0..nAir-1]
  let sortedUniqueAirKeys = dedup $ mapMaybe (isAir Nothing Just) $ S.toList v
      expectedAirKeys
        | nAir == 0 = []
        | otherwise = [0.. fromIntegral $ nAir-1]
  when (sortedUniqueAirKeys /= expectedAirKeys) $ do
    onError
    fail $ name ++ ":air keys are wrong:" ++ show(expectedAirKeys,sortedUniqueAirKeys)
  return ()

generateAtLeastN :: Int -> IO [a] -> IO [a]
generateAtLeastN n act =
  go n []
 where
  go remaining l
   | remaining <= 0 = return l
   | otherwise = act >>= \generated -> go (remaining - length generated) (generated ++ l)

countAirElements :: SmallMatInfo -> IO Int
countAirElements m@(SmallMatInfo nAir l) = do
  print m
  mapM_ putStrLn $ showInBox' $ writeWorld m
  let nAir' = length $ filter ((== Air).materialAndKeyToMaterial) $ concat $ Cyclic.toLists l
  verifyMat "countAirElements" m
  if nAir == nAir'
    then return nAir
    else fail $ "mismatch nAir" ++ show(nAir, nAir')

-- Test deactivated, it actually fails, and shows that we need to make sure
-- the sum of lower bounds is smaller than the total number of blocks, else
-- it is guaranteed that no world can be generated.
{-
testMinimas :: IO ()
testMinimas =
  forM_ [0..8] $ \i ->
    forM_ [0..8] $ \j -> do
      let sz = Size i j
          nBlocks = area sz
      forM_ [0..64] $ \n -> do
        print (i,j,n)
        (fromMaybe 0 (minCountAirBlocks n sz) +
          fromMaybe 0 (minCountWallBlocks n sz)) `shouldBeSmallerOrEqualTo` nBlocks

shouldBeSmallerOrEqualTo :: (Show a, Ord a) => a -> a -> IO ()
shouldBeSmallerOrEqualTo actual maxValue =
  unless (actual <= maxValue) $ error $ "expected <= \n" ++ show maxValue ++ " but got\n" ++ show actual
-}

matricesWithoutAirOnEveryFronteer :: [(ComponentCount,Unboxed.Matrix Material)]
matricesWithoutAirOnEveryFronteer = map (fmap readWorld) [
 (1,
 ["     "
 ,"     "
 ,"    O"])
 ,
 (1,
 ["    O"
 ,"     "
 ,"     "])
 ,
 (2,
 ["  O  "
 ,"     "
 ,"O    "])
 ,
 (2,
 ["    O"
 ,"O    "
 ,"     "])
 ,
 (3,
 ["     "
 ,"O   O"
 ,"  O  "])
 ,
 (3,
 ["  O  "
 ,"O    "
 ,"  O  "])
 ,
 (3,
 ["  O  "
 ,"O   O"
 ,"     "])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"  O  "])
 ,
 (1,
 ["OOOO "
 ,"OOOO "
 ,"OOOO "])
 ,
 (1,
 ["     "
 ,"OOOOO"
 ,"OOOOO"])
 ,
 (1,
 [" OOOO"
 ," OOOO"
 ," OOOO"])
 ,
 (1,
 ["OOOOO"
 ,"OOOOO"
 ,"     "])
 ,
 (1,
 ["OOOOOOOOOOOOOOOOO"
 ,"OOOOOOOOOOOOOOOOO"
 ,"                 "])
 ]

matricesWithAirOnEveryFronteer :: [(ComponentCount,Unboxed.Matrix Material)]
matricesWithAirOnEveryFronteer = map (fmap readWorld) [
 (2,
 ["O    "
 ,"     "
 ,"    O"])
 ,
 (2,
 ["    O"
 ,"     "
 ,"O    "])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O    "])
 ,
 (3,
 ["    O"
 ,"O    "
 ,"  O  "])
 ,
 (4,
 ["  O  "
 ,"O   O"
 ,"  O  "])
 ,
 (1,
 ["OOOOO"
 ,"O   O"
 ,"OOOOO"])
 ,
 (1,
 ["OOOOO"
 ,"OOOOO"
 ,"OOOOO"])
 ,
 (1,
 ["OOOOOOOOOOOOOOOOOOOO"
 ,"OOOOOOOOOOOOOOOOOOOO"
 ,"OOOOOOOOOOOOOOOOOOOO"])
 ]


matricesWithNComponents :: [(ComponentCount,Unboxed.Matrix Material)]
matricesWithNComponents = map (fmap readWorld) [
 (2,
 ["O    "
 ,"     "
 ,"    O"])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O    "])
 ,
 (4,
 ["  O  "
 ,"O   O"
 ,"  O  "])
 ,
 (8,
 ["O O O"
 ," O O "
 ,"O O O"])
 ,
 (1,
 ["OOOOO"
 ,"O   O"
 ,"OOOOO"])
 ,
 (1,
 ["OOOOO"
 ,"OOOOO"
 ,"OOOOO"])
 ,
 (1,
 ["OOOOO"
 ,"O   O"
 ,"O OOO"
 ,"O    "
 ,"OOOOO"
 ,"    O"
 ,"OOO O"
 ,"O   O"
 ,"OOOOO"])
 ,
 (1,
 ["O OOO"
 ,"O O O"
 ,"OOO O"])
 ,
 (1,
 ["O OOOOOOOOOOOOOOOOOO"
 ,"O O                O"
 ,"OOO                O"])
 ]

matricesWithNWellDistributedComponentsSpaceNotWellUsed :: [(ComponentCount,Unboxed.Matrix Material)]
matricesWithNWellDistributedComponentsSpaceNotWellUsed = map (fmap readWorld) [
 (2,
 ["O    "
 ,"     "
 ,"    O"])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O    "])
 ,
 (4,
 ["  O  "
 ,"O   O"
 ,"  O  "])
 ,
 (2,
 ["O  O "
 ,"O  OO"
 ,"O  O "])
 ,
 (2,
 ["O   O"
 ,"OO  O"
 ,"OO  O"])
 ,
 (2,
 ["OOO  "
 ,"    O"
 ,"   OO"])
 ,
 (2,
 ["   OO"
 ,"    O"
 ,"OOO  "])
 ,
 (2,
 ["  OOO"
 ,"O    "
 ,"OO   "])
 ,
 (2,
 ["OO   "
 ,"O    "
 ,"  OOO"])
 ,
 (2,
 ["OOOOOOOO         "
 ,"      O          "
 ,"        OOOOOOOOO"])
 ]

matricesWithNWellDistributedComponentsSpaceWellUsed :: [(ComponentCount,Unboxed.Matrix Material)]
matricesWithNWellDistributedComponentsSpaceWellUsed = map (fmap readWorld) [
 (8,
 ["O O O"
 ," O O "
 ,"O O O"])
 ,
 (5,
 ["O   O"
 ," O O "
 ,"  O  "])
 ,
 (5,
 ["  O  "
 ," O O "
 ,"O   O"])
 ,
 (8,
 ["  O  "
 ," O O "
 ,"O   O"
 ," O O "
 ,"  O  "])
 ,
 (9,
 ["O   O"
 ," O O "
 ,"  O  "
 ," O O "
 ,"O   O"])
 ,
 (7,
 ["O O O"
 ,"     "
 ,"    O"
 ," O   "
 ,"  O O"])
 ,
 (7,
 ["O O  "
 ,"   O "
 ,"    O"
 ,"   O "
 ,"O O  "])
 ,
 (7,
 ["O   O"
 ,"     "
 ,"O   O"
 ," O O "
 ,"  O  "])
 ,
 (2,
 ["OOOOO"
 ,"     "
 ,"OOOOO"])
 ,
 (3,
 ["OO OO"
 ,"O O O"
 ,"O OO "])
 ,
 (3,
 ["O O O"
 ,"O O O"
 ,"O O O"])
 ,
 (2,
 ["OOO  "
 ,"   OO"
 ,"    O"])
 ,
 (2,
 ["    O"
 ,"   OO"
 ,"OOO  "])
 ,
 (2,
 ["  OOO"
 ,"OO   "
 ,"O    "])
 ,
 (2,
 ["O    "
 ,"OO   "
 ,"  OOO"])
 ,
 (5,
 ["OOOOO"
 ,"     "
 ,"O OOO"
 ,"O    "
 ,"OO O "
 ,"   OO"
 ,"  O O"
 ,"OOO O"
 ,"  O  "])
 ,
 (2,
 ["O OOO"
 ,"O O O"
 ,"OO  O"])
 ,
 (2,
 ["O      OOOOOOOOO"
 ,"O      O O      "
 ,"OOOOOOO  O      "])
 ]

matricesWithNNotWellDistributedComponents :: [(ComponentCount,Unboxed.Matrix Material)]
matricesWithNNotWellDistributedComponents = map (fmap readWorld) [
 (2,
 ["OOO  "
 ,"     "
 ,"    O"])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O  OO"])
 ,
 (4,
 ["O O O"
 ,"O   O"
 ,"O OO "])
 ,
 (6,
 ["O OOO"
 ," O O "
 ,"O O O"])
 ,
 (2,
 ["OO   "
 ,"     "
 ,"OOOOO"])
 ,
 (3,
 ["OOOOO"
 ,"O   O"
 ,"O OOO"
 ,"     "
 ,"O OOO"
 ,"    O"
 ,"OOO O"
 ,"O   O"
 ,"OOOOO"])
 ]



shouldNotBe :: (Show a, Eq a) => a -> a -> IO ()
shouldNotBe actual unexpected =
  when (actual == unexpected) $ fail $ "didn't expect\n" ++ show unexpected


shouldBeBiggerOrEqualTo :: (Show a, Ord a) => a -> a -> IO ()
shouldBeBiggerOrEqualTo actual minValue =
  unless (actual >= minValue) $ fail $ "expected >= \n" ++ show minValue ++ " but got\n" ++ show actual

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  unless (actual == expected) $ fail $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
