{-# LANGUAGE NoImplicitPrelude #-}

module Render (
          renderChar
        , renderPoints
        , renderColored
        , Alignment(..)
        , renderAlignedTxt
        , renderAlignedTxt_
        , go
        , Render.move
        , translate
        , mkRenderStateToCenterWorld
        -- | reexports
        , Coords(..)
        , Row(..)
        , Col(..)
        , Direction(..)
        , RenderState(..)
        , Color8Code(..)
        ) where

import           Imajuscule.Prelude

import           Data.Text( Text, length )

import qualified System.Console.Terminal.Size as Terminal( size
                                                         , Window(..))

import           Console( RenderState(..)
                        , renderChar_
                        , renderTxt_
                        , setRawForeground
                        , restoreForeground
                        , Color8Code(..) )
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

-- | Minimal margin between the upper left corner of the console
--   and upper left corner of the world
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

renderColored :: Char -> [Coords] -> Color8Code -> RenderState -> IO ()
renderColored char points colorCode state = do
  fg <- setRawForeground colorCode
  renderPoints char state points
  restoreForeground fg

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: Alignment -> Text -> RenderState -> IO ()
renderAlignedTxt_ a txt ref = do
  let amount = case a of
        Centered     -> 1 + quot (length txt) 2
        RightAligned -> length txt
      leftCorner = Render.move amount LEFT ref
  renderTxt_ txt leftCorner

renderAlignedTxt :: Alignment -> Text -> RenderState -> IO RenderState
renderAlignedTxt a txt ref =
  renderAlignedTxt_ a txt ref >> return (go Down ref)

mkRenderStateToCenterWorld :: WorldSize -> IO RenderState
mkRenderStateToCenterWorld s =
  worldUpperLeftToCenterIt' s <$> Terminal.size
