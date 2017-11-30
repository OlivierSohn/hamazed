{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game(
        runGameWorker
      ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.List( minimumBy, find, foldl', notElem )
import           Data.Maybe( catMaybes
                           , isNothing )
import           Data.String(String)
import           Data.Text( pack, singleton )

import           Animation
import           Animation.Design.Chars
import           Animation.Types

import           Color

import           Game.Deadline( Deadline(..) )
import           Game.Event
import           Game.Level
import           Game.Parameters( GameParameters(..) )
import           Game.World
import           Game.World.Frame
import           Game.World.Laser
import           Game.World.Number
import           Game.World.Size
import           Game.World.Space

import           Geo.Conversion
import           Geo.Discrete.Types
import           Geo.Continuous
import           Geo.Discrete hiding(translate)

import           Render.Console
import           Render

import           Timing
import           Util

data GameState = GameState {
    _gameStateStartTime :: !Timer
  , _gameStateNextMotionStep :: !(Maybe KeyTime)
  , _gameStateEmbeddedWorld :: !EmbeddedWorld
  , _gameStateworld :: !World
  , _gameStateShotNumbers :: ![Int]
  , _gameStateTarget :: !Int
  , _gameStateLevel :: !Level
}

-- TODO move code for animations creation out
nextGameState :: GameState -> TimedEvent -> GameState
nextGameState
  (GameState a b d world@(World balls f bs@(BattleShip (PosSpeed shipCoords shipSpeed) ammo safeTime collisions) space@(Space mat mayAnim sz u) animations) g target level@(Level i finished))
  te@(TimedEvent event t) =
    maybe
      normal
      (const $ error "not supposed to have an animation here")
        mayAnim
  where
       (maybeLaserRayTheoretical, newAmmo) =
         if ammo > 0 then case event of
           (Action Laser dir) ->
             (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite (`location` space), pred ammo)
           _ ->
             (Nothing, ammo)
         else
           (Nothing, ammo)
       ((remainingBalls', destroyedBalls), maybeLaserRay) =
         maybe
           ((balls,[]), Nothing)
           (survivingNumbers balls RayDestroysFirst)
             maybeLaserRayTheoretical

       remainingBalls = case event of
         Timeout GameStep _ ->
           if isNothing safeTime
             then
               filter (`notElem` collisions) remainingBalls'
             else
               remainingBalls'
         _ -> remainingBalls'

       keyTime = KeyTime t
       eventAnims = case event of
            Timeout GameStep k ->
              if not (null collisions) && isNothing safeTime
                then
                  -- number and ship explode, but they exchange speeds
                  let collidingNumbersSpeed = foldl' sumCoords zeroCoords $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
                      (Number _ n) = head collisions
                  in  map ((`BoundedAnimation` WorldFrame) .
                            (\(char,f) -> mkAnimation f k SkipZero (Speed 1) (Just char))) $
                          map ((,) '|')            (explosion (speed2vec collidingNumbersSpeed) shipCoords) ++
                          map ((,) $ intToDigit n) (explosion (speed2vec shipSpeed) shipCoords)
                else
                  []
            _ -> []

       laserRayAnim =
         maybe
           []
           (\ray -> [BoundedAnimation (mkLaserAnimation keyTime ray) WorldFrame])
             maybeLaserRay

       outerWorldAnims =
         if not $ null destroyedBalls
           then
              []
           else
             maybe
               []
               (\ray@(LaserRay dir _) ->
                  let ae = afterEnd ray
                      fr = onFronteer ae sz
                      borderExplosions = case fr of
                        Just outDir -> let speed = assert (dir == outDir) (scalarProd 2 $ speed2vec $ coordsForDirection outDir)
                                           explosions = explosionGravity speed $ translateInDir outDir ae
                                       in map (((`BoundedAnimation` TerminalWindow) .
                                                (\ (char, f) -> mkAnimation f keyTime WithZero (Speed 1) (Just char))) .
                                                  ((,) $ niceChar $ getSeconds t))
                                                    explosions
                        Nothing -> []
                  in borderExplosions ++ [BoundedAnimation (mkLaserAnimation keyTime ray) WorldFrame]
               ) maybeLaserRay

       newAnimations =
         destroyNumbersAnimations destroyedBalls event keyTime
         ++ eventAnims
         ++ laserRayAnim
         ++ outerWorldAnims
         ++ animations

       newWorld = nextWorld world remainingBalls newAmmo newAnimations
       destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
       allShotNumbers = g ++ destroyedNumbers
       newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
       newLevel = Level i newFinished
       normal = GameState a b d newWorld allShotNumbers target newLevel

destroyNumbersAnimations :: [Number] -> Event -> KeyTime -> [BoundedAnimation]
destroyNumbersAnimations nums event keyTime =
  let sp = case event of
        (Action Laser dir) -> speed2vec $ coordsForDirection dir
        _                  -> Vec2 0 0
      animation pos = map (\f -> (f, Speed 2)) (explosion (scalarProd 2 sp) pos)
  in case nums of
    Number (PosSpeed pos _) n:_ ->
      let animations = animation pos ++ [(animatedNumber n (mkAnimationTree pos Traverse), Speed 1)]
          create (f,speed) = mkAnimation f keyTime SkipZero speed $ Just $ intToDigit n
      in  map (\a -> BoundedAnimation (create a) WorldFrame) animations
    _ -> []

replaceAnimations :: [BoundedAnimation] -> GameState -> GameState
replaceAnimations anims (GameState a c e (World wa wb wc wd _) f g h) =
  GameState a c e (World wa wb wc wd anims) f g h

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
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ _ _ level) t =
  maybe
    (catMaybes [messageDeadline level t, gameDeadline s, animationDeadline s])
    (: [])
      (frameAnimationDeadline s)

gameDeadline :: GameState -> Maybe Deadline
gameDeadline (GameState _ nextGameStep _ _ _ _ (Level _ levelFinished)) =
  maybe
    (maybe
      Nothing
      (\s -> Just $ Deadline s GameStep)
        nextGameStep)
    (const Nothing)
      levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ _ _ world _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti AnimationStep) $ earliestAnimationDeadline world

frameAnimationDeadline :: GameState -> Maybe Deadline
frameAnimationDeadline (GameState _ _ _ world _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti FrameAnimationStep) $ earliestFrameAnimationDeadline world



accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState a c d (World wa wb ship wc wd) f g h) =
  let newShip = accelerateShip dir ship
      world = World wa wb newShip wc wd
  in GameState a c d world f g h


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

runGameWorker :: GameParameters -> IO ()
runGameWorker params =
  makeInitialState params firstLevel Nothing
    >>= \case
            Left err -> error err
            Right ew -> loop params ew

makeInitialState :: GameParameters -> Int -> Maybe WorldSize -> IO (Either String GameState)
makeInitialState (GameParameters shape wallType) level mayPrevSize = do
  let numbers = [1..(3+level)] -- more and more numbers as level increases
      worldSize = worldSizeFromLevel level shape
  eew <- mkEmbeddedWorld worldSize
  case eew of
    Left err -> return $ Left err
    Right ew -> do
      world <- mkWorld mayPrevSize worldSize wallType numbers
      t <- getCurrentTime
      let kt = maybe (Just $ KeyTime t) (const Nothing) mayPrevSize
      return $ Right $ GameState (Timer t) kt ew world [] (sum numbers `quot` 2) $ Level level Nothing

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
getEvent state@(GameState _ _ _ _ _ _ level) = do
  t <- getCurrentTime
  let deadline = nextDeadline state t
  getEventForMaybeDeadline level deadline t

updateGameUsingTimedEvent :: GameParameters -> GameState -> TimedEvent -> IO GameState
updateGameUsingTimedEvent
 params
 state@(GameState a b d world@(World _ _ _ (Space _ _ prevSz _) _) f g h@(Level level mayLevelFinished))
 te@(TimedEvent event t) =
  case event of
    Nonsense -> return state
    StartLevel nextLevel ->
      -- TODO make an animation between current world size and next
      makeInitialState params nextLevel (Just prevSz)
        >>= \case
              Left err -> error err
              Right s -> return s
    _ -> do
          let newState = case event of
                (Timeout FrameAnimationStep _) -> updateAnim t state
                (Timeout GameStep gt) -> GameState a (Just $ addGameStepDuration gt) d (moveWorld t world) f g h
                (Timeout MessageStep _) -> -- TODO this part is ugly, we should not have to deduce so much
                                         -- MessageStep is probably the wrong abstraction level
                  case mayLevelFinished of
                    Just (LevelFinished stop finishTime _) -> GameState a b d world f g $ Level level (Just $ LevelFinished stop finishTime ContinueMessage)
                    Nothing -> state
                _ -> state
          updateGame2 te newState


updateAnim :: UTCTime -> GameState -> GameState
updateAnim
  t
  (GameState a b c (World d e f (Space g mayAnim sz h) i) j k l)
   = maybe
       (error "should not happen")
       (\(FrameAnimation prevSize it@(Iteration (_, Frame count)) deadline) ->
           let (newGameStep, newAnim) =
                 if count < maxNumberOfSteps prevSize sz
                   then
                     (Nothing, Just $ FrameAnimation prevSize (nextIteration it) $ addFrameAnimationStepDuration deadline)
                   else
                     (Just $ KeyTime t, Nothing)
           in GameState a newGameStep c (World d e f (Space g newAnim sz h) i) j k l
       ) mayAnim

updateGame2 :: TimedEvent -> GameState -> IO GameState
updateGame2 te@(TimedEvent event _) s =
  case event of
    Action Ship dir -> return $ accelerateShip' dir s
    _ -> do
      beginFrame
      let s2 =
            maybe
              (nextGameState s te)
              (const s)
                $ frameAnimation s
      animations <- renderGame (getKeyTime event) s2
      endFrame
      return $ replaceAnimations animations s2

frameAnimation :: GameState -> Maybe FrameAnimation
frameAnimation (GameState _ _ _ (World _ _ _ (Space _ anim _ _) _) _ _ _) = anim

renderGame :: Maybe KeyTime -> GameState -> IO [BoundedAnimation]
renderGame k state@(GameState _ _ (EmbeddedWorld mayTermWindow curUpperLeft)
                   (World _ _ (BattleShip _ ammo _ _) space@(Space _ mayAnim curSz _) animations)
                   shotNumbers target (Level level _)) = do
  -- while we are animating the frame, the layout needs to be like it was before
  let (sz@(WorldSize (Coords (Row rs) (Col cs))), upperLeft) =
        maybe
            (curSz, curUpperLeft)
            (\(FrameAnimation prevSz _ _) ->
              let d@(RenderState (Coords _ (Col dc))) = diffUpperLeft curSz prevSz
              in if dc >= 0
                  then
                    -- animation expands the frame
                    (curSz, curUpperLeft)
                  else
                    -- animation shrinks the frame
                    (prevSz, sumRS d curUpperLeft)
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
  (do
    _ <- renderAlignedTxt Centered ("Level " <> pack (show level) <> " of " <> pack (show lastLevel)) centerDown
    _ <- go Down <$> renderAligned RightAligned (colored (singleton '[') bracketsColor
                                              <> colored (pack $ replicate ammo '.') ammoColor
                                              <> colored (singleton ']') bracketsColor) leftMiddle
         >>= renderAligned RightAligned (showShotNumbers shotNumbers)
    _ <- renderAlignedTxt Centered ("Objective : " <> pack (show target)) centerUp
    return ())

  -- We render the world using curUpperLeft because it's the new world
  -- If instead we decide to render the old world while animationg the frame we should
  -- pass upperLeft instead
  renderSpace space curUpperLeft >>=
    (\worldCorner -> do
        activeAnimations <- renderAnimations k space mayTermWindow worldCorner animations
        renderWorldAndLevel state worldCorner
        renderWorldFrame mayAnim curSz curUpperLeft -- render it last so that when it animates
                                                    -- to reduce, it goes over numbers and ship
        return activeAnimations)

locationFunction :: Boundaries
                 -> Space
                 -> Maybe (Window Int)
                 -> RenderState
                 -> (Coords -> Location)
locationFunction f space@(Space _ _ sz _) mayTermWindow (RenderState wcc) =
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
renderWorldAndLevel (GameState _ _ _
                   world@(World _ _ _ (Space _ _ (WorldSize (Coords (Row rs) (Col cs))) _) _)
                   _ _ level) worldCorner = do
  renderWorld world worldCorner
  let
    rightMiddle = translate (Row (quot rs 2)) (Col $ cs + 2) worldCorner
  renderLevel level rightMiddle
