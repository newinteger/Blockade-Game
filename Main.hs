{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (Left, Right, Up, Down)
import UI.NCurses
import Control.Concurrent (threadDelay)
import Control.Monad.State.Lazy (liftIO)

import Model
import Init
import View
import Collision

main :: IO ()
main = runCurses $ do setEcho False
                      w <- defaultWindow
                      updateWindow w $ do clear
                                          moveCursor 10 10
                                          drawBoundry
                      render
                      let players = spawnPlayer [[(5, 5)], [(5, 50)]]
                      gameLoop w players
                      liftIO $ threadDelay 300000000

gameLoop :: Window -> [Player] -> Curses ()
gameLoop w players = loop players
                    where
                        loop players = do collided <- checkCollisionBoundaries players w `andM` checkCollisionPlayers players
                                          if collided then
                                            do newPlayers <- doWeChangeDirs players w
                                               newPlayers <- updateWindow w $ do p <- movePlayers newPlayers
                                                                                 draw p
                                                                                 return p
                                               render
                                               gameLoop w newPlayers
                                           else 
                                             do updateWindow w $ do gameOver
                                                render                                                         

-- Move a player in their direction and add their previous position to their position history
movePlayer :: Player -> Player
movePlayer p@Player{..} = p { pos = updatePosBasedOnDir (head pos) dir : pos, dir = dir }

-- Move each player in their direction
movePlayers :: [Player] -> Update [Player]
movePlayers xs = return (map movePlayer xs)

-- Checks for input
-- If appropriate input is given the player direction is changed
doWeChangeDirs ::  [Player] -> Window -> Curses [Player]
doWeChangeDirs [x,y] w = do liftIO $ threadDelay 100000
                            ev <- getEvent w (Just 0)
                            case ev of
                             Nothing                              -> return [x,y]
                             Just (EventCharacter 'w')            -> return [changeDir x Up, y]
                             Just (EventCharacter 'a')            -> return [changeDir x Left, y]
                             Just (EventCharacter 's')            -> return [changeDir x Down, y]
                             Just (EventCharacter 'd')            -> return [changeDir x Right, y]
                             Just (EventSpecialKey KeyUpArrow)    -> return [x, changeDir y Up]
                             Just (EventSpecialKey KeyLeftArrow)  -> return [x, changeDir y Left]
                             Just (EventSpecialKey KeyDownArrow)  -> return [x, changeDir y Down]
                             Just (EventSpecialKey KeyRightArrow) -> return [x, changeDir y Right]
                             Just (EventCharacter _)              -> return [x,y]

-- Changes the direction of a player
changeDir :: Player -> Direction -> Player
changeDir p@Player{..} newDir = p { pos = pos, dir = newDir }

-- Updates a player position based on the direction they are facing
-- Note that origo is in the top left corner, that going downwards is a positive increase in position and that a position is (row, column)
updatePosBasedOnDir :: GridPos -> Direction -> GridPos
updatePosBasedOnDir (x,y) Left  = (x, y -1)
updatePosBasedOnDir (x,y) Up    = (x - 1, y)
updatePosBasedOnDir (x,y) Down  = (x + 1, y)
updatePosBasedOnDir (x,y) Right = (x, y + 1)

-- Monadic version of and, hoogle link: https://hoogle.haskell.org/?hoogle=andM
andM :: Monad m => m Bool -> m Bool -> m Bool
andM mx my = do x <- mx
                if x
                then my
                else return x