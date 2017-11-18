module Render (
          renderChar
        , renderPoints
        , Alignment(..)
        , renderAlignedStr
        , go
        , Render.move
        , translate
        , mkRenderStateToCenterWorld
        -- | reexports
        , Coords(..)
        , RenderState(..)
        ) where

import           Imajuscule.Prelude

import qualified System.Console.Terminal.Size as Terminal( size
                                                         , Window(..))

import           Console( RenderState(..)
                        , renderChar_
                        , renderStr_ )
import           Geo( Coords(..)
                    , Direction(..)
                    , Row(..)
                    , Col(..)
                    , move
                    , sumCoords
                    , translateInDir )
import           WorldSize( WorldSize(..) )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

-- | Mininmal margin between the uuper left corner of the console
--   and the upper left corner of the world
minimalWorldMargin :: Int
minimalWorldMargin = 4

go :: Direction -> RenderState -> RenderState
go dir (RenderState r) = RenderState $ translateInDir dir r

move :: Int -> Direction -> RenderState -> RenderState
move n dir (RenderState c) = RenderState $ Geo.move n dir c

translate :: Row -> Col -> RenderState -> RenderState
translate r c (RenderState coords) = RenderState $ sumCoords coords $ Coords r c

worldUpperLeftToCenterIt' :: WorldSize -> Maybe (Terminal.Window Int) -> RenderState
worldUpperLeftToCenterIt' worldSize termSize =
   RenderState $ maybe
     (Coords (Row minimalWorldMargin) (Col minimalWorldMargin))
     (`worldUpperLeftFromTermSize` worldSize)
     termSize

worldUpperLeftFromTermSize :: Terminal.Window Int -> WorldSize -> Coords
worldUpperLeftFromTermSize (Terminal.Window h w) (WorldSize (Coords (Row rs) (Col cs))) =
  let walls = 2 :: Int
  in Coords (Row $ quot (h-(rs+walls)) 2) (Col $ quot (w-(cs+walls)) 2)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

renderChar :: Char -> Coords -> RenderState -> IO ()
renderChar char pos (RenderState upperLeftCoords) =
  renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords

renderPoints :: Char -> RenderState -> [Coords] -> IO ()
renderPoints char state =
  mapM_ (\c -> renderChar char c state)

data Alignment = Centered
               | RightAligned

renderAlignedStr :: Alignment -> String -> RenderState -> IO RenderState
renderAlignedStr a str ref = do
  let amount = case a of
        Centered     -> quot (length str) 2
        RightAligned -> length str
      leftCorner = Render.move amount LEFT ref
  renderStr_ str leftCorner
  return $ go Down ref

mkRenderStateToCenterWorld :: WorldSize -> IO RenderState
mkRenderStateToCenterWorld s =
  worldUpperLeftToCenterIt' s <$> Terminal.size
