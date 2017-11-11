{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where


import           Control.Concurrent( threadDelay )
import           Control.Exception( finally )
import           Control.Monad.Loops( unfoldM )
import           Data.Char( intToDigit )
import           Data.List( partition )
import           Data.Maybe( fromMaybe
                           , isNothing
                           , maybe )
import           Data.Time( UTCTime
                          , diffUTCTime
                          , getCurrentTime )
import           System.Console.ANSI( clearScreen )
import           System.IO( getChar
                          , hFlush
                          , hReady
                          , stdin
                          , stdout )
import           System.Timeout( timeout )

import           Console( configureConsoleFor
                        , ConsoleConfig(..)
                        , renderChar_
                        , renderSegment
                        , renderStrLn
                        , RenderState(..) )
import           Geo( sumCoords
                    , Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , segmentContains
                    , translateCoord
                    , zeroCoords )
import           Laser( LaserType(..)
                      , laserChar
                      , Ray(..)
                      , Theoretical
                      , Actual
                      , shootLaserFromShip
                      , stopRayAtFirstCollision )
import           Threading( runAndWaitForTermination
                          , Termination(..) )
import           Timing( addMotionStepDuration
                       , computeTime
                       , nextUpdateCounter
                       , showUpdateTick
                       , Timer(..) )
import           World( Action(..)
                      , ActionTarget(..)
                      , actionFromChar
                      , BattleShip(..)
                      , Location(..)
                      , location
                      , moveWorld
                      , mkWorld
                      , nextWorld
                      , World(..)
                      , Number(..)
                      , worldSize )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !Timer
  , _nextMotionStep :: !UTCTime
  , _updateCounter :: !Int
  , _upperLeftCorner :: !Coords
  , _world :: !World
  , _gameStateLaserRay :: !(Maybe (LaserRay Actual))
  , _gameShotNumbers :: ![Int]
  , _gameTarget :: !Int
}

data GameStops = Lost String
               | Won

data LaserRay a = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !(Ray a)
}

data LaserPolicy = RayDestroysFirst | RayDestroysAll

nextGameState :: GameState -> Action -> (GameState, Maybe GameStops)
nextGameState (GameState a b c d world@(World balls _ (BattleShip (PosSpeed shipCoords _) ammo)) _ g h) action =
  let (maybeLaserRayTheoretical, newAmmo) = if ammo > 0 then case action of
          (Action Laser dir) -> (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite, pred ammo)
          _     -> (Nothing, ammo)
        else (Nothing, ammo)
      ((remainingBalls, destroyedBalls), maybeLaserRay) = maybe ((balls,[]), Nothing) (survivingNumbers balls RayDestroysFirst) maybeLaserRayTheoretical
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      sumNumbers = sum allShotNumbers
      normal = case compare sumNumbers h of
        LT -> Nothing
        EQ -> Just Won
        GT -> Just $ Lost $ show sumNumbers ++ " is bigger than " ++ show h
  in (GameState a b c d (nextWorld action world remainingBalls newAmmo) maybeLaserRay allShotNumbers h,
      case action of
        Timeout ->
          if shipCollides world
            then Just $ Lost "collision"
            else normal
        _ -> normal)


survivingNumbers :: [Number] -> LaserPolicy -> LaserRay Theoretical -> (([Number],[Number]), Maybe (LaserRay Actual))
survivingNumbers l policy (LaserRay dir theoreticalRay@(Ray seg)) = case policy of
  RayDestroysAll -> (partition (\(Number (PosSpeed pos _) _) -> (isNothing $ segmentContains pos seg)) l, justFull)
  RayDestroysFirst ->
    let (rayActual, mayCoord) = stopRayAtFirstCollision (map (\(Number (PosSpeed pos _) _) -> pos) l) theoreticalRay
        remainingNumbers = case mayCoord of
          Nothing -> (l,[])
          (Just pos') -> partition (\(Number (PosSpeed pos _) _) -> pos /= pos') l
    in (remainingNumbers, Just $ LaserRay dir rayActual)
 where
   justFull = Just $ LaserRay dir $ Ray seg

shipCollides :: World -> Bool
shipCollides (World balls _ (BattleShip (PosSpeed shipCoords _) _)) =
   any (\(Number (PosSpeed pos _) _) -> shipCoords == pos) balls

showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime _ updateTick _ _ _ _ _) =
  let time = computeTime startTime currentTime
  in "|" ++ showUpdateTick updateTick ++ "| " ++ show time ++ " |"

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO Termination
run =
  (configureConsoleFor Gaming >> runAndWaitForTermination gameWorker)
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  `finally`
   configureConsoleFor Editing


gameWorker :: IO ()
gameWorker = makeInitialState >>= loop


makeInitialState :: IO GameState
makeInitialState = do
  t <- getCurrentTime
  world <- mkWorld
  return $ GameState (Timer t) t 0 zeroCoords world Nothing [] 15


loop :: GameState -> IO ()
loop state@(GameState _ _ _ coords _ _ _ _) = do
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  renderStrLn (showTimer t s) r


updateGame :: GameState -> RenderState -> IO GameState
updateGame state@(GameState a b c d world f g h) r =
  getAction state >>=
    (\action -> case action of
      Nonsense -> return state
      _        -> updateGame2 action r =<<
        (case action of
          Timeout ->
            getCurrentTime >>= (\t ->
              return $ GameState a (addMotionStepDuration t) (nextUpdateCounter c) d (moveWorld world) f g h)
          (Action Frame dir) ->
            return $ GameState a b c (sumCoords d $ coordsForDirection dir) world f g h
          _        -> return state
        ))

updateGame2 :: Action -> RenderState -> GameState -> IO GameState
updateGame2 a r s = do
  clearScreen
  let (s2, termination) = nextGameState s a
  renderGame s2 r
  maybeS3 <- mapM (handle s2) termination
  let s3 = fromMaybe s2 maybeS3
  hFlush stdout
  return s3


handle :: GameState -> GameStops -> IO GameState
handle _ stop = do
  putStrLn $ case stop of
    (Lost reason) -> " You Lose (" ++ reason ++ ")"
    Won           -> " You Win!! Congratulations."
  hFlush stdout -- write previous messages
  threadDelay $ 2 * 1000000
  putStrLn "Press a key to continue ..."
  unfoldM readOneChar -- flush inputs
  hFlush stdout -- write previous message
  getChar       -- wait for a key press
  makeInitialState

readOneChar :: IO (Maybe Char)
readOneChar = do
  hasMore <- hReady stdin
  if hasMore
    then do
      c <- getChar
      return $ Just c
    else
      return Nothing

{--
getActions :: IO [Action]
getActions = do
  inputs <- unfoldM readOneChar
  return $ map actionFromChar inputs
--}


getAction :: GameState -> IO Action
getAction (GameState _ nextMotionStep _ _ _ _ _ _) = do
  t <- getCurrentTime
  let remainingSeconds = diffUTCTime nextMotionStep t
      remainingMicros = floor (remainingSeconds * 10^(6 :: Int))
  if remainingMicros < 0
    then return Timeout
    else
      (\case
        (Just (Just c)) -> actionFromChar c
        (Just _) -> Nonsense
        _ -> Timeout
        ) <$> timeout remainingMicros readOneChar


renderGame :: GameState -> RenderState -> IO ()
renderGame state@(GameState _ _ _ _ (World balls _ (BattleShip (PosSpeed shipCoords _) ammo)) maybeLaserRay shotNumbers target) frameCorner = do
  _ <- printTimer state frameCorner >>= (\r -> do
    _ <- renderAmmo ammo r >>= renderTarget target >>= renderShotNumbers shotNumbers
    renderWorldFrame r >>= (\worldCorner -> do
      _ <- case maybeLaserRay of
        (Just (LaserRay laserDir (Ray laserSeg))) -> renderSegment laserSeg (laserChar laserDir) worldCorner
        Nothing -> return worldCorner
      -- render numbers, including the ones that will be destroyed, if any
      mapM_ (\(Number (PosSpeed pos _) i) -> render_ (intToDigit i) pos worldCorner) balls
      render_ '+' shipCoords worldCorner))
  return ()


renderShotNumbers :: [Int] -> RenderState -> IO RenderState
renderShotNumbers nums =
  renderStrLn ("Shot: " ++ show nums)

renderTarget :: Int -> RenderState -> IO RenderState
renderTarget n =
  renderStrLn ("Target: " ++ show n)


renderAmmo :: Int -> RenderState -> IO RenderState
renderAmmo ammo (RenderState upperLeftCoords) = do
  let vmargin = 1
      hmargin = 1
      translate = Coords (Row vmargin) (Col $ worldSize+2+hmargin)
      corner = sumCoords upperLeftCoords translate
  renderStrLn ("Ammo: " ++ show ammo) $ RenderState corner

renderWorldFrame :: RenderState -> IO RenderState
renderWorldFrame upperLeft@(RenderState upperLeftCoords) = do
  let horizontalWall = replicate (worldSize + 2)
      lowerLeft = RenderState $ sumCoords upperLeftCoords $ Coords (Row $ worldSize+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLeft
  let worldCoords = translateCoord RIGHT renderCoords

  -- left & right walls
  let leftWallCoords = take worldSize $ iterate (translateCoord Down) renderCoords
      toRight = Coords (Row 0) (Col $ worldSize+1)
      rightWallCoords = take worldSize $ iterate (translateCoord Down) $ sumCoords renderCoords toRight
  mapM_ (renderChar_ '|' . RenderState) (leftWallCoords ++ rightWallCoords)

  -- lower wall
  _ <- renderStrLn (horizontalWall 'T') lowerLeft
  return $ RenderState worldCoords


render_ :: Char -> Coords -> RenderState -> IO ()
render_ char worldCoords (RenderState renderCoords) =
  case location worldCoords of
    InsideWorld -> renderChar_ char loc
    _           -> return ()
  where loc = RenderState $ sumCoords renderCoords worldCoords
