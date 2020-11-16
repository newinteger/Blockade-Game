module Init where

import Prelude hiding (Left, Down)
import Model
import UI.NCurses

-- Create a new player
spawnPlayer :: [[GridPos]] -> [Player]
spawnPlayer = map (\x -> Player {col = ColorRed, pos = x, dir = Down})