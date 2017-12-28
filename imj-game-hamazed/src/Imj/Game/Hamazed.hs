{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}


module Imj.Game.Hamazed
      ( -- * The game
        {-| In Hamazed, you are a 'BattleShip' pilot surrounded by flying 'Number's.

        Your mission is to shoot exactly the 'Number's whose sum will equate the
        current 'Level' 's /target number/.

        The higher the 'Level' (1..12), the more 'Number's are flying around (up-to 16).
        And the smaller the 'World' gets.

        Good luck !
        -}
          run
        -- * Game loop
        {-| Hamazed is a /synchronous/, /event-driven/ program. Its /simplified/ main loop is:

        * 'getNextDeadline'

            * compute \(deadline\) = the next foreseen 'Deadline' to handle, given a 'GameState'.

        * 'getEventForMaybeDeadline'

            * From \(deadline\), deduce \(timeout\) = max wait time for a player event
            * Wait for the \(player\) to press a valid key for \(timeout\) seconds.

                * If no \(player\) @event@ occured before \(timeout\) seconds
                , \(timestampedEvent\) = \(deadline\) @event@
                * Else, \(timestampedEvent\) = \(player\) @event@

        * 'updateAndRender'

            * Update 'GameState' according to \(timestampedEvent\)
            * Render if needed, using "Imj.Render.Delta" to avoid
            <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.
        -}
      , getNextDeadline
      , getEventForMaybeDeadline
      , updateAndRender
        -- * Deadlines
      , Deadline(..)
      , DeadlineType(..)
      -- ** Overdue deadlines priorities
      -- | When multiple overdue deadlines are competing, the following priorities apply:
      --
      -- 'AnimateUI' > 'DisplayContinueMessage' > 'MoveFlyingItems' > /Player event/ > 'Animate'
      , deadlinePriority
      , playerEventPriority
        -- * Timestamped Events
        -- | Every 'Event' is timestamped with the time at which it was generated:
      , TimestampedEvent(..)
        -- * Events
      , Event(..)
      , ActionTarget(..)
      , MetaAction(..)
        -- * GameState
        {-| 'GameState' has two fields of type 'World' : during 'Level' transitions,
        we render the /old/ 'World' while using the /new/ 'World' 's
        dimensions to animate the UI accordingly (see "Imj.UI.Animation"). -} -- TODO this could be done differently
      , GameState(..)
        -- * Environment
        {- | -}
      , module Imj.Game.Hamazed.Env
        -- * Utilities
      , eventFromKey
      , getKeyTime
      ) where

import           Imj.Prelude
import qualified Prelude (putStrLn)

import           System.Info(os)

import           Control.Monad.Reader(runReaderT)

import           Data.List( minimumBy, find )
import           Data.Maybe( catMaybes )

import           Imj.Animation
import           Imj.Animation.Design hiding (earliestDeadline)
import           Imj.Game.Element.Laser
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.Event
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.Render
import           Imj.Game.Hamazed.Timing
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.World.Number
import           Imj.Game.Hamazed.World.Ship
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.UI.RectContainer
import           Imj.Physics.Discrete.Collision
import           Imj.Render.Delta
import           Imj.Threading

{- | Runs the Hamazed game.

If your current terminal window is too small, the program will error and
tell you what is the minimum window size to run the game.

The game doesn't run on Windows, because with GHC,
<https://ghc.haskell.org/trac/ghc/ticket/7353 IO operations cannot be interrupted on Windows>.
-}
run :: IO ()
run =
  if os == "mingw32"
    then
      Prelude.putStrLn $ "Windows is not currently supported,"
      ++ " due to this GHC bug: https://ghc.haskell.org/trac/ghc/ticket/7353."
    else
      void doRun

doRun :: IO Termination
doRun =
  runThenRestoreConsoleSettings
    (createEnv >>= runAndWaitForTermination . runReaderT gameWorker)

{-# INLINABLE gameWorker #-}
gameWorker :: (Draw e, MonadReader e m, MonadIO m)
           => m ()
gameWorker =
  getGameParameters >>= runGameWorker


nextGameState :: GameState
              -> TimestampedEvent
              -> GameState
nextGameState
  (GameState b world@(World _ ship@(BattleShip posspeed ammo safeTime collisions) space animations e) futureWorld
             g (Level i target finished) (UIAnimation (UIEvolutions j upDown left) k l))
  te@(TimestampedEvent event t) =
  let (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) = eventAction event world
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
         ++ maybe [] (`laserAnims` keyTime) maybeLaserRay
         ++ outerSpaceAnims_
         ++ animations

      newWorld = World remainingBalls (BattleShip posspeed newAmmo safeTime collisions) space newAnimations e
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newLeft =
        if null destroyedNumbers && ammo == newAmmo
          then
            left
          else
            let frameSpace = mkWorldContainer worldFrameColors world
                infos = mkLeftInfo Normal newAmmo allShotNumbers
                (_, _, leftMiddle) = getSideCentersAtDistance frameSpace 2
            in mkTextAnimRightAligned leftMiddle leftMiddle infos 0 -- 0 duration, since animation is over anyway
      newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
      newLevel = Level i target newFinished
      newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
  in assert (isFinished newAnim) $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim


outerSpaceAnims :: KeyTime
                -> Space
                -> LaserRay Actual
                -> [BoundedAnimation]
outerSpaceAnims k (Space _ sz _) ray@(LaserRay dir _) =
  let laserTarget = afterEnd ray
  in case onOuterBorder laserTarget sz of
       Just outDir -> outerSpaceAnims' k laserTarget $ assert (dir == outDir) dir
       Nothing -> []

outerSpaceAnims' :: KeyTime
                 -> Coords Pos
                 -> Direction
                 -> [BoundedAnimation]
outerSpaceAnims' keyTime@(KeyTime (MkSystemTime _ nanos)) fronteerPoint dir =
  let char = niceChar $ fromIntegral nanos -- cycle character every nano second
      speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
      outerSpacePoint = translateInDir dir fronteerPoint
      anims = fragmentsFreeFall speed outerSpacePoint keyTime (Speed 1) char
  in map (`BoundedAnimation` TerminalWindow) anims


laserAnims :: LaserRay Actual
           -> KeyTime
           -> [BoundedAnimation]
laserAnims keyTime ray
 = [BoundedAnimation (laserAnimation keyTime ray) WorldFrame]

replaceAnimations :: [BoundedAnimation] -> GameState -> GameState
replaceAnimations anims (GameState c (World wa wc wd _ ew) b f g h) =
  GameState c (World wa wc wd anims ew) b f g h

{- | Returns the next 'Deadline' to handle.

We prefer having time-accurate game motions for central items of the game
(the 'BattleShip', the 'Number's) than having time-accurate explosive 'Animation's.

Hence, when multiple overdue deadlines are competing, the following priorities apply
(higher number = higher priority):

\[
\newcommand\T{\Rule{0pt}{.5em}{.3em}}
  \begin{array}{|c|c|c|}
	\hline
  \textbf{ Priority } \T & \textbf{ Name     } \T & \textbf{ Description                            } \\\hline
	\text{ 5 } & \text{ AnimateUI              } \T & \text{ Inter-level animations                   } \\\hline
	\text{ 4 } & \text{ DisplayContinueMessage } \T & \textit{ Press a key to continue                } \\\hline
  \text{ 3 } & \text{ MoveFlyingItems        } \T & \text{ Move the BattleShip and Numbers          } \\\hline
  \text{ 2 } & \textit{ Player event         } \T & \text{ Handle a key-press                       } \\\hline
  \text{ 1 } & \text{ Animate                } \T & \text{ Update animations (explosions and others)} \\\hline
	\end{array}
\]

Applying these priorities to overdue 'Deadline's, a long-overdue 'Animate' deadline
could be ignored in favor of a recent 'MoveFlyingItems' deadline.

Note that if no 'Deadline' is overdue (they all happen in the future), we return
the closest one in time, irrespective of its priority.

We /could/ apply priorities for non-overdue deadlines, too. For example if a
'MoveFlyingItems' very closely follows an 'Animate' (say, 15 millisecond after),
we could swap their order so as to have a better guarantee that the game motion
will happen in-time and not be delayed by a potentially heavy animation update.
But it's very unlikely that it will make a difference, except if updating
the 'Animation's becomes /very/ slow for some reason.
-}
getNextDeadline :: GameState
                -- ^ Current state
                -> SystemTime
                -- ^ The current time.
                -> Maybe Deadline
getNextDeadline s t =
  let l = getDeadlinesByDecreasingPriority s t
  in  overdueDeadline t l <|> earliestDeadline l

earliestDeadline :: [Deadline] -> Maybe Deadline
earliestDeadline [] = Nothing
earliestDeadline l  = Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

overdueDeadline :: SystemTime -> [Deadline] -> Maybe Deadline
overdueDeadline t = find (\(Deadline (KeyTime t') _) -> t' < t)

-- | priorities are : uiAnimation > message > game > player key > animation
getDeadlinesByDecreasingPriority :: GameState -> SystemTime -> [Deadline]
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ level _) t =
  catMaybes [ uiAnimationDeadline s
            , messageDeadline level t
            , getMoveFlyingItemsDeadline s
            , animationDeadline s
            ]

getMoveFlyingItemsDeadline :: GameState -> Maybe Deadline
getMoveFlyingItemsDeadline (GameState nextGameStep _ _ _ (Level _ _ levelFinished) _) =
  maybe
    (maybe
      Nothing
      (\s -> Just $ Deadline s MoveFlyingItems)
        nextGameStep)
    (const Nothing)
      levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ world _ _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti Animate) $ earliestAnimationDeadline world

uiAnimationDeadline :: GameState -> Maybe Deadline
uiAnimationDeadline (GameState _ _ _ _ _ (UIAnimation _ mayDeadline _)) =
  maybe
    Nothing
    (\deadline -> Just $ Deadline deadline AnimateUI)
      mayDeadline


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState c (World wa ship wc wd we) b f g h) =
  let newShip = accelerateShip dir ship
      world = World wa newShip wc wd we
  in GameState c world b f g h


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


{-# INLINABLE runGameWorker #-}
runGameWorker :: (Draw e, MonadReader e m, MonadIO m)
              => GameParameters
              -> m ()
runGameWorker params =
  mkInitialState params firstLevel Nothing
    >>= \case
      Left err -> error err
      Right ew -> loop params ew

mkInitialState :: (MonadIO m)
               => GameParameters
               -> Int
               -> Maybe GameState
               -> m (Either String GameState)
mkInitialState (GameParameters shape wallType) levelNumber mayState = do
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
      newLevel = Level levelNumber target Nothing
      newSize = worldSizeFromLevel levelNumber shape
      newAmmo = 10
      newShotNums = []
      make ew = do
        newWorld <- mkWorld ew newSize wallType numbers newAmmo
        t <- liftIO getSystemTime
        let (curWorld, level, ammo, shotNums) =
              maybe
              (newWorld, newLevel, 0, [])
              (\(GameState _ w@(World _ (BattleShip _ curAmmo _ _) _ _ _)
                           _ curShotNums curLevel _) ->
                  (w, curLevel, curAmmo, curShotNums))
                mayState
            curInfos = mkInfos Normal ammo shotNums level
            newInfos = mkInfos ColorAnimated newAmmo newShotNums newLevel
            uiAnimation =
              mkUIAnimation
                (mkWorldContainer worldFrameColors curWorld, curInfos)
                (mkWorldContainer worldFrameColors newWorld, newInfos)
                t
            gameDeadline =
              if isFinished uiAnimation
                then
                  Just $ KeyTime t
                else
                  Nothing
        return $ Right $ GameState gameDeadline curWorld newWorld newShotNums newLevel uiAnimation
  mkInTerminal newSize >>= either (return . Left) make


{-# INLINABLE loop #-}
loop :: (Draw e, MonadReader e m, MonadIO m)
     => GameParameters
     -> GameState
     -> m ()
loop params state =
  updateGame params state >>= (\(st, mayMeta) ->
    maybe (loop params st) (const $ return ()) mayMeta)


{-# INLINABLE updateGame #-}
updateGame :: (Draw e, MonadReader e m, MonadIO m)
           => GameParameters
           -> GameState
           -> m (GameState, Maybe MetaAction)
updateGame params state = do
  evt <- liftIO $ getTimedEvent state
  case evt of
    TimestampedEvent (Interrupt i) _ -> return (state, Just i)
    _ -> do
      st <- updateAndRender params state evt
      return (st, Nothing)

getTimedEvent :: GameState -> IO TimestampedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getSystemTime
    return $ TimestampedEvent evt t

getEvent :: GameState -> IO Event
getEvent state = do
  mayEvent <- getEvent' state
  case mayEvent of
    Just event -> return event
    Nothing -> getEvent state

getEvent' :: GameState -> IO (Maybe Event)
getEvent' state@(GameState _ _ _ _ level _) = do
  t <- getSystemTime
  let deadline = getNextDeadline state t
  getEventForMaybeDeadline level deadline t

-- | Updates the 'GameState', if needed, and renders using "Imj.Render.Delta"
-- to avoid <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.
{-# INLINABLE updateAndRender #-}
updateAndRender :: (Draw e, MonadReader e m, MonadIO m)  -- TODO This function should be split in two : update / render.
                => GameParameters
                -- ^ 'World' creation parameters
                -- (will be used in case the 'Event' is 'StartLevel')
                -> GameState
                -- ^ The current state
                -> TimestampedEvent
                -- ^ The 'TimestampedEvent' that should be handled here.
                -> m GameState
updateAndRender
 params
 state@(GameState b world futWorld f h@(Level level target mayLevelFinished) i)
 te@(TimestampedEvent event t) =
  case event of
    StartLevel nextLevel ->
      mkInitialState params nextLevel (Just state)
        >>= \case
              Left err -> error err
              Right s -> return s
    _ -> do
          let newState = case event of
                (Timeout (Deadline _ AnimateUI))
                  -> updateAnim t state
                (Timeout (Deadline gt MoveFlyingItems))
                  -> GameState (Just $ addDuration gameMotionPeriod gt) (updateWorld t world) futWorld f h i
                (Timeout (Deadline _ DisplayContinueMessage)) ->
                  case mayLevelFinished of
                    Just (LevelFinished stop finishTime _) ->
                      let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
                      in GameState b world futWorld f newLevel i
                    Nothing -> state
                _ -> state
          updateGame2 te newState


updateAnim :: SystemTime -> GameState -> GameState
updateAnim t (GameState _ curWorld futWorld j k (UIAnimation evolutions _ it)) =
     let nextIt@(Iteration _ nextFrame) = nextIteration it
         (world, gameDeadline, worldAnimDeadline) =
            maybe
              (futWorld , Just $ KeyTime t, Nothing)
              (\dt ->
               (curWorld, Nothing         , Just $ KeyTime $ addToSystemTime (floatSecondsToDiffTime dt) t))
              $ getDeltaTime evolutions nextFrame
         wa = UIAnimation evolutions worldAnimDeadline nextIt
     in GameState gameDeadline world futWorld j k wa


{-# INLINABLE updateGame2 #-}
updateGame2 :: (Draw e, MonadReader e m, MonadIO m)
            => TimestampedEvent
            -> GameState
            -> m GameState
updateGame2
 te@(TimestampedEvent event _)
 s@(GameState _ _ _ _ _ anim) =
  case event of
    Action Ship dir ->
      return $ accelerateShip' dir s
    _ -> do
      let s2 =
            if isFinished anim
              then
                nextGameState s te
              else
                s
      animations <- renderGame (getKeyTime event) s2
      renderDrawing
      return $ replaceAnimations animations s2


{-# INLINABLE renderGame #-}
renderGame :: (Draw e, MonadReader e m, MonadIO m)
           => Maybe KeyTime
           -> GameState
           -> m [BoundedAnimation]
renderGame k (GameState _ world@(World _ _ space@(Space _ (Size rs cs) _)
                                       animations (InTerminal mayTermWindow curUpperLeft))
                        _ _ level wa) =
  renderSpace space curUpperLeft >>=
    (\worldCorner -> do
        activeAnimations <- renderAnimations k space mayTermWindow worldCorner animations
        -- TODO merge 2 functions below (and no need to pass worldCorner)
        renderWorld world
        renderLevelMessage level (translate' (quot rs 2) (cs + 2) worldCorner)
        renderUIAnimation wa -- render it last so that when it animates
                                  -- to reduce, it goes over numbers and ship
        return activeAnimations)

{-# INLINABLE renderAnimations #-}
renderAnimations :: (Draw e, MonadReader e m, MonadIO m)
                 => Maybe KeyTime
                 -> Space
                 -> Maybe (Window Int)
                 -> Coords Pos
                 -> [BoundedAnimation]
                 -> m [BoundedAnimation]
renderAnimations k space mayTermWindow worldCorner animations = do
  let renderAnimation (BoundedAnimation a scope) = do
        let interaction = scopedLocation space mayTermWindow worldCorner scope
                           >>> \case
                                InsideWorld  -> Stable
                                OutsideWorld -> Mutation
            a' = updateAnimationIfNeeded k interaction a
        renderAnim a' interaction colorFromFrame worldCorner
          >>= \case
            True -> return $ Just $ BoundedAnimation a' scope
            False -> return Nothing
  catMaybes <$> mapM renderAnimation animations
