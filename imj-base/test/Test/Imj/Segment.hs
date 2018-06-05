module Test.Imj.Segment
         ( testSegment
         ) where

import           Control.Monad(when)

import           Imj.Geo.Discrete

-- | returns 'True' on success, else errors
testSegment :: IO Bool
testSegment = do
  let horizontal = mkSegment (Coords 0 0) (Coords 0 2)
      hCount = countSegmentElements horizontal
      eH = extremities horizontal
  when (hCount /= 3) $ error $ "horizontal count = " ++ show hCount
  when (eH /= (Coords 0 0, Coords 0 2)) $ error $ "horizontal extremities = " ++ show eH

  let vertical = mkSegment (Coords 0 0) (Coords (-2) 0)
      vCount = countSegmentElements vertical
      eV = extremities vertical
  when (vCount /= 3) $ error $ "vertical count = " ++ show vCount
  when (eV /= (Coords 0 0, Coords (-2) 0)) $ error $ "vertical extremities = " ++ show eV

  let oblique = mkSegment (Coords 0 0) (Coords (-2) 2)
      oCount = countSegmentElements oblique
      eO = extremities oblique
  when (oCount /= 3) $ error $ "oblique count = " ++ show oCount
  when (eO /= (Coords 0 0, Coords (-2) 2)) $ error $ "oblique extremities = " ++ show eO

  let horizontal' = changeSegmentCount 1 horizontal
      vertical' = changeSegmentCount 1 vertical
      oblique' = changeSegmentCount 1 oblique
      hCount2 = countSegmentElements horizontal'
      vCount2 = countSegmentElements vertical'
      oCount2 = countSegmentElements oblique'
  when (hCount2 /= 1) $ error $ "new horizontal count = " ++ show hCount2
  when (vCount2 /= 1) $ error $ "new vertical count = " ++ show vCount2
  when (oCount2 /= 1) $ error $ "new oblique count = " ++ show oCount2

  mapM_
    (\(supposedCount, segment) -> do
        let count = length $ bresenham segment
        when (count /= supposedCount) $ error $ show (count, supposedCount, segment))
    $ zip
      [hCount, vCount, oCount, hCount2, vCount2, oCount2]
      [horizontal, vertical, oblique, horizontal', vertical', oblique']
  return True
