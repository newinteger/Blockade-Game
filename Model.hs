module Model where

import UI.NCurses (Color)


type GridPos = (Integer, Integer)
data Player = Player { col  :: Color, pos  :: [GridPos], dir :: Direction }
  deriving (Eq, Show)

data Direction = Up | Down  | Left | Right
  deriving (Eq, Show, Enum, Bounded)