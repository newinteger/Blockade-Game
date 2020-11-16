module Collision where

import Prelude hiding (Up, Down, Left, Right)
import Control.Monad.Extra (allM)
import UI.NCurses

import Model

-- Check for all players if any have collided with the boundry
checkCollisionBoundaries :: [Player] -> Window -> Curses Bool
checkCollisionBoundaries xs w = do allM (checkCollisionBoundry w) xs

-- Check if a player is in bounds or not (1 before)
checkCollisionBoundry :: Window -> Player -> Curses Bool
checkCollisionBoundry w (Player _ pos Up)    = enclosed w (fst (head pos) - 1) (snd (head pos))
checkCollisionBoundry w (Player _ pos Down)  = enclosed w (fst (head pos) + 1) (snd (head pos))
checkCollisionBoundry w (Player _ pos Right) = enclosed w (fst (head pos))     (snd (head pos) + 1)
checkCollisionBoundry w (Player _ pos Left)  = enclosed w (fst (head pos))     (snd (head pos) - 1)

-- Check if players have collided with themselves or with eachother
checkCollisionPlayers :: [Player] -> Curses Bool
checkCollisionPlayers [Player _ pos1 _,Player _ pos2 _] | (chk (head pos1) pos2 || chk (head pos1) (tail pos1)) || (chk (head pos2) pos1 || chk (head pos2) (tail pos2)) = return False
                                                        | otherwise = return True
                                                         where chk t y = tupleInTupleList t y

-- Check if 2 touples are identical
compareTuples :: GridPos -> GridPos -> Bool
compareTuples (x,y) (x',y') | x == x' && y == y' = True
                            | otherwise          = False

-- Check if a tuple exists in a list?
tupleInTupleList :: GridPos -> [GridPos] -> Bool
tupleInTupleList t xs = any (compareTuples t) xs