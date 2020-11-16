{-# LANGUAGE FlexibleInstances #-}

module View where

import Model
import Prelude hiding (Left, Right, Up, Down)
import UI.NCurses

-- Writes out game over
gameOver :: Update ()
gameOver = do moveCursor 0 0
              drawString "------- GAME OVER -------"

-- Draws the boundry of the arena
drawBoundry :: Update ()
drawBoundry = drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

class Drawable a where
    draw :: a -> Update ()

-- Draw the new player head and replace their old head with a body piece
instance Drawable Player where
    draw (Player _ pos Up)    = do uncurry moveCursor (head pos)
                                   drawString "^"
                                   uncurry moveCursor (pos!!1)
                                   drawString "#" 
    draw (Player _ pos Down)  = do uncurry moveCursor (head pos)
                                   drawString "v"
                                   uncurry moveCursor (pos!!1)
                                   drawString "#" 
    draw (Player _ pos Left)  = do uncurry moveCursor (head pos)
                                   drawString "<"
                                   uncurry moveCursor (pos!!1)
                                   drawString "#" 
    draw (Player _ pos Right) = do uncurry moveCursor (head pos)
                                   drawString ">"
                                   uncurry moveCursor (pos!!1)
                                   drawString "#" 
                                   
-- Use the flexibibleinstances extension to create a function that
-- draws all the players in the list at their respecitve positions correctly
-- mapM, the monadic version of map, which doesn't return something
instance Drawable [Player] where
    draw xs = do mapM_ draw xs