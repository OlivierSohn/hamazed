{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game(
        runGameWorker
      ) where

import           Imajuscule.Prelude

import           Data.List( minimumBy, find )
import           Data.Maybe( catMaybes, isNothing, fromMaybe )
import           Data.String(String)
import           Data.Text( pack, singleton )

import           Animation
import           Animation.Design.Chars

import           Color

import           Game.Deadline( Deadline(..) )
import           Game.World.Ship
import           Game.Event
import           Game.Level
import           Game.Parameters( GameParameters(..) )
import           Game.World
import           Game.World.Embedded
import           Game.World.Frame
import           Game.World.Laser
import           Game.World.Number
import           Game.World.Size
import           Game.World.Space

import           Geo.Conversion
import           Geo.Discrete.Types
import           Geo.Continuous
import           Geo.Discrete hiding(translate)

import           Math

import           Render.Console
import           Render

import           Timing
import           Util

data GameState = GameState {
    _gameStateStartTime :: !Timer
  , _gameStateNextMotionStep :: !(Maybe KeyTime)
  , _gameStateWorld :: !World
  , _gameStateShotNumbers :: ![Int]
  , _gameStateTarget :: !Int
  , _gameStateLevel :: !Level
  , _gameStateFrameAnimation :: !(Maybe FrameAnimation)
}

nextGameState :: GameState -> TimedEvent -> GameState
nextGameState
  (GameState a b world@(World _ _ ship space animations _)
             g target (Level i finished) mayAnim)
  te@(TimedEvent event t) =
  let (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) = withLaserAction event world

      keyTime = KeyTime t

      outerSpaceAnims_ =
         if null destroyedBalls
           then
             maybe [] (outerSpaceAnims keyTime space) maybeLaserRay
           else
            []

      newAnimations =
            destroyedNumbersAnimations keyTime event destroyedBalls
         ++ shipAnims ship event
         ++ maybe [] (laserAnims keyTime) maybeLaserRay
         ++ outerSpaceAnims_
         ++ animations

      newWorld = nextWorld world remainingBalls newAmmo newAnimations
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
      newLevel = Level i newFinished
  in assert (isNothing mayAnim) GameState a b newWorld allShotNumbers target newLevel mayAnim


outerSpaceAnims :: KeyTime -> Space -> LaserRay Actual -> [BoundedAnimation]
outerSpaceAnims keyTime@(KeyTime t) (Space _ sz _) ray@(LaserRay dir _) =
  let ae = afterEnd ray
  in case onFronteer ae sz of
        Just outDir -> let speed = assert (dir == outDir) (scalarProd 2 $ speed2vec $ coordsForDirection outDir)
                           explosions = explosionGravity speed $ translateInDir outDir ae
                       in map (((`BoundedAnimation` TerminalWindow) .
                                (\ (char, f) -> mkAnimation f keyTime WithZero (Speed 1) (Just char))) .
                                  ((,) $ niceChar $ getSeconds t))
                                    explosions
        Nothing -> []


laserAnims :: KeyTime -> LaserRay Actual -> [BoundedAnimation]
laserAnims keyTime ray
 = [BoundedAnimation (mkLaserAnimation keyTime ray) WorldFrame]

replaceAnimations :: [BoundedAnimation] -> GameState -> GameState
replaceAnimations anims (GameState a c (World wa wb wc wd _ ew) f g h i) =
  GameState a c (World wa wb wc wd anims ew) f g h i

nextDeadline :: GameState -> UTCTime -> Maybe Deadline
nextDeadline s t =
  let l = getDeadlinesByDecreasingPriority s t
  in  overdueDeadline t l <|> earliestDeadline l

earliestDeadline :: [Deadline] -> Maybe Deadline
earliestDeadline [] = Nothing
earliestDeadline l  = Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

overdueDeadline :: UTCTime -> [Deadline] -> Maybe Deadline
overdueDeadline t = find (\(Deadline (KeyTime t') _) -> t' < t)

-- | priorities are : message > game forward > animation forward
getDeadlinesByDecreasingPriority :: GameState -> UTCTime -> [Deadline]
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ _ level _) t =
  maybe
    (catMaybes [messageDeadline level t, gameDeadline s, animationDeadline s])
    (: [])
      (frameAnimationDeadline s)

gameDeadline :: GameState -> Maybe Deadline
gameDeadline (GameState _ nextGameStep _ _ _ (Level _ levelFinished) _) =
  maybe
    (maybe
      Nothing
      (\s -> Just $ Deadline s GameStep)
        nextGameStep)
    (const Nothing)
      levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ _ world _ _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti AnimationStep) $ earliestAnimationDeadline world

frameAnimationDeadline :: GameState -> Maybe Deadline
frameAnimationDeadline (GameState _ _ _ _ _ _ mayFrameAnim) =
  maybe
    Nothing
    (\(FrameAnimation _ _ _ _ _ mayDeadline)
        -> maybe
             Nothing
             (\deadline -> Just $ Deadline deadline FrameAnimationStep)
               mayDeadline
    ) mayFrameAnim


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState a c (World wa wb ship wc wd we) f g h i) =
  let newShip = accelerateShip dir ship
      world = World wa wb newShip wc wd we
  in GameState a c world f g h i


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

runGameWorker :: GameParameters -> IO ()
runGameWorker params =
  makeInitialState params firstLevel Nothing
    >>= \case
            Left err -> error err
            Right ew -> loop params ew

makeInitialState :: GameParameters -> Int -> Maybe World -> IO (Either String GameState)
makeInitialState
 (GameParameters shape wallType) level mayCurWorld = do
  let numbers = [1..(3+level)] -- more and more numbers as level increases
      nextSize = worldSizeFromLevel level shape
  eew <- mkEmbeddedWorld nextSize
  case eew of
    Left err -> return $ Left err
    Right ew -> do
      newWorld <- mkWorld ew nextSize wallType numbers
      t <- getCurrentTime
      let frameAnimation =
            maybe
              Nothing
              (\(World _ _ _ (Space _ curSz _) _ _) ->
                  if curSz == nextSize
                    then
                      Nothing
                    else
                      Just $ mkFrameAnimation newWorld t invQuartEaseInOut (maxNumberOfSteps curSz nextSize)
              ) mayCurWorld
          (kt, world) =
            maybe
              (Just $ KeyTime t, newWorld)
              (const (Nothing, fromMaybe (error "shoud not happen") mayCurWorld))
                frameAnimation
      return $ Right $ GameState (Timer t) kt world [] (sum numbers `quot` 2) (Level level Nothing) frameAnimation

loop :: GameParameters -> GameState -> IO ()
loop params state =
  updateGame params state >>= (\(st, mayMeta) ->
    maybe (loop params st) (const $ return ()) mayMeta)

updateGame :: GameParameters -> GameState -> IO (GameState, Maybe Meta)
updateGame params state = getTimedEvent state >>= (\evt ->
  case evt of
    TimedEvent (Interrupt i) _ -> return (state, Just i)
    _                 -> do
      st <- updateGameUsingTimedEvent params state evt
      return (st, Nothing))

getTimedEvent :: GameState -> IO TimedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getCurrentTime
    return $ TimedEvent evt t

getEvent :: GameState -> IO Event
getEvent state@(GameState _ _ _ _ _ level _) = do
  t <- getCurrentTime
  let deadline = nextDeadline state t
  getEventForMaybeDeadline level deadline t

updateGameUsingTimedEvent :: GameParameters -> GameState -> TimedEvent -> IO GameState
updateGameUsingTimedEvent
 params
 state@(GameState a b world f g h@(Level level mayLevelFinished) i)
 te@(TimedEvent event t) =
  case event of
    Nonsense -> return state
    StartLevel nextLevel ->
      makeInitialState params nextLevel (Just world)
        >>= \case
              Left err -> error err
              Right s -> return s
    _ -> do
          let newState = case event of
                (Timeout FrameAnimationStep _) -> updateAnim t state
                (Timeout GameStep gt) -> GameState a (Just $ addGameStepDuration gt) (moveWorld t world) f g h i
                (Timeout MessageStep _) -> -- TODO this part is ugly, we should not have to deduce so much
                                         -- MessageStep is probably the wrong abstraction level
                  case mayLevelFinished of
                    Just (LevelFinished stop finishTime _) ->
                      let newLevel = Level level (Just $ LevelFinished stop finishTime ContinueMessage)
                      in GameState a b world f g newLevel i
                    Nothing -> state
                _ -> state
          updateGame2 te newState


updateAnim :: UTCTime -> GameState -> GameState
updateAnim
  t
  (GameState a _ curWorld j k l mayAnim)
   = maybe
       (error "should not happen")
       (\(FrameAnimation nextWorld_ startTime ease nsteps it _) ->
         -- nsteps is the number of steps including start and end steps.
           let nextIt@(Iteration (_, Frame nextCount)) = nextIteration it
               (newGameStep, newAnim, world) =
                 if nextCount < nsteps
                   then do
                     let ratio = (0.5 + fromIntegral (assert(nextCount <= nsteps - 1) nextCount)) / fromIntegral nsteps
                         time = floatSecondsToNominalDiffTime $ animationDuration * ease (assert (ratio <= 1.0 && ratio >= 0.0) ratio)
                         animationDuration = 1.8
                         deadline = Just $ KeyTime $ addUTCTime time startTime
                     (Nothing,
                      Just $ FrameAnimation nextWorld_ startTime ease nsteps nextIt deadline,
                      curWorld)
                   else
                     (Just $ KeyTime t,
                      Nothing,
                      nextWorld_) -- TODO adjust timing if needed so that the game starts earlier or later
           in GameState a newGameStep world j k l newAnim
       ) mayAnim

updateGame2 :: TimedEvent -> GameState -> IO GameState
updateGame2 te@(TimedEvent event _) s@(GameState _ _ _ _ _ _ mayAnim) =
  case event of
    Action Ship dir -> return $ accelerateShip' dir s
    _ -> do
      beginFrame
      let s2 =
            maybe
              (nextGameState s te)
              (const s)
                mayAnim
      animations <- renderGame (getKeyTime event) s2
      endFrame
      return $ replaceAnimations animations s2

-- | When a frame animation is in progress, we layout according to the largest size
--   to make the animation more visible
renderGame :: Maybe KeyTime -> GameState -> IO [BoundedAnimation]
renderGame k state@(GameState _ _ curWorld@(World _ _
                                                 (BattleShip _ ammo _ _)
                                                 space@(Space _ curSz _) animations
                                                 (EmbeddedWorld mayTermWindow curUpperLeft))
                   shotNumbers target (Level level _) mayAnim) = do
  let (WorldSize (Coords (Row rs) (Col cs)), upperLeft) =
        maybe
            (curSz, curUpperLeft)
            (\(FrameAnimation (World _ _ _ (Space _ nextSz _) _ (EmbeddedWorld _ nextUpperLeft)) _ _ _ _ _) ->
              let (RenderState (Coords _ (Col dc))) = diffRS curUpperLeft nextUpperLeft
              in if dc >= 0
                  then
                    -- animation expands the frame
                    (nextSz, nextUpperLeft)
                  else
                    -- animation shrinks the frame
                    (curSz, curUpperLeft)
            ) mayAnim
      addWallSize = (+ 2)
      half = flip quot 2
      mkSizes s = (addWallSize s, half s)
      (rFull, rHalf) = mkSizes rs
      (_    , cHalf) = mkSizes cs

      centerUp   = translate (Row $ -1)        (Col $ cHalf + 1) upperLeft
      centerDown = translate (Row $ rFull + 1) (Col $ cHalf + 1) upperLeft
      leftMiddle = translate (Row $ rHalf + 1) (Col $ -1)  upperLeft
  -- TODO animate this
  do
    _ <- renderAlignedTxt Centered ("Level " <> pack (show level) <> " of " <> pack (show lastLevel)) centerDown
    _ <- go Down <$> renderAligned RightAligned (colored (singleton '[') bracketsColor
                                              <> colored (pack $ replicate ammo '.') ammoColor
                                              <> colored (singleton ']') bracketsColor) leftMiddle
         >>= renderAligned RightAligned (showShotNumbers shotNumbers)
    _ <- renderAlignedTxt Centered ("Objective : " <> pack (show target)) centerUp
    return ()

  -- We render the world using curUpperLeft because it's the new world
  -- If instead we decide to render the old world while animationg the frame we should
  -- pass upperLeft instead
  renderSpace space curUpperLeft >>=
    (\worldCorner -> do
        activeAnimations <- renderAnimations k space mayTermWindow worldCorner animations
        renderWorldAndLevel state worldCorner
        renderWorldFrame mayAnim curWorld -- render it last so that when it animates
                                               -- to reduce, it goes over numbers and ship
        return activeAnimations)

locationFunction :: Boundaries
                 -> Space
                 -> Maybe (Window Int)
                 -> RenderState
                 -> (Coords -> Location)
locationFunction f space@(Space _ sz _) mayTermWindow (RenderState wcc) =
  let worldLocation = (`location` space)
      worldLocationExcludingBorders = (`strictLocation` space)
      terminalLocation (Window h w) coordsInWorld =
        let (Coords (Row r) (Col c)) = sumCoords coordsInWorld wcc
        in if r >= 0 && r < h && c >= 0 && c < w
             then
               InsideWorld
             else
               OutsideWorld
      productLocations l l' = case l of
        InsideWorld -> l'
        OutsideWorld -> OutsideWorld

  in case f of
    WorldFrame -> worldLocation
    TerminalWindow -> maybe
                        worldLocation
                        (\wd coo-> if contains coo sz then OutsideWorld else terminalLocation wd coo)
                        mayTermWindow
    Both       -> maybe
                    worldLocation
                    (\wd coo-> productLocations (terminalLocation wd coo) (worldLocationExcludingBorders coo))
                    mayTermWindow

renderAnimations :: Maybe KeyTime
                 -> Space
                 -> Maybe (Window Int)
                 -> RenderState
                 -> [BoundedAnimation]
                 -> IO [BoundedAnimation]
renderAnimations k space mayTermWindow worldCorner animations = do
  let renderAnimation (BoundedAnimation a@(Animation _ _ _ render) f) = do
        let fLocation = locationFunction f space mayTermWindow worldCorner
        fmap (`BoundedAnimation` f) <$> render k a fLocation worldCorner
  activeAnimations <- mapM renderAnimation animations
  let res = catMaybes activeAnimations
  return res

renderWorldAndLevel :: GameState -> RenderState -> IO ()
renderWorldAndLevel (GameState _ _
                   world@(World _ _ _ (Space _ (WorldSize (Coords (Row rs) (Col cs))) _) _ _)
                   _ _ level _) worldCorner = do
  renderWorld world
  let
    rightMiddle = translate (Row (quot rs 2)) (Col $ cs + 2) worldCorner
  renderLevel level rightMiddle
