module World
    ( Action(..)
    , ActionTarget(..)
    , actionFromChar
    , extend
    , Location(..)
    , location
    , worldSize
    ) where


import           Geo( Col(..)
                    , Coords(..)
                    , Direction(..)
                    , Row(..))

data Action = Action ActionTarget Direction |
              Timeout |
              Nonsense |
              Throw deriving (Show)

data ActionTarget = Frame | Ship | Laser deriving(Eq, Show)

data Location = InsideWorld | OutsideWorld

actionFromChar :: Char -> Action
actionFromChar c = case c of
  'o' -> Throw
  'g' -> Action Frame Down
  't' -> Action Frame Up
  'f' -> Action Frame LEFT
  'h' -> Action Frame RIGHT
  'k' -> Action Laser Down
  'i' -> Action Laser Up
  'j' -> Action Laser LEFT
  'l' -> Action Laser RIGHT
  's' -> Action Ship Down
  'w' -> Action Ship Up
  'a' -> Action Ship LEFT
  'd' -> Action Ship RIGHT
  _   -> Nonsense


worldSize :: Int
worldSize = 35


location :: Coords -> Location
location (Coords (Row r) (Col c))
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize


extend :: Coords -> Direction -> Coords
extend (Coords (Row r) (Col c)) dir = case dir of
  Up    -> Coords (Row 0) (Col c)
  LEFT  -> Coords (Row r) (Col 0)
  Down  -> Coords (Row (worldSize-1)) (Col c)
  RIGHT -> Coords (Row r) (Col (worldSize-1))
