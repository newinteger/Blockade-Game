* The game begins with a rectuangular playing field that is divided into squares where each players start position is repressented by an arrow.
* For each time step a wall is built at the arrows position and the arrow takes a step in its direction.
* The player can by pressing one of four keys change directions of the arrow. 
* The object of the game is to build as big of a wall as possible without colliding with the enclosing rectangle, the other players wall or your own wall.

###### Take note that the ncurses library is needed.

## Building/Running locally:

```bash
cd <path-to-dir>/Blockade-Game
cabal sandbox init
cabal install ncurses

ghci Main.hs
```

And then run 
```
main
```

## References
* Package: https://hackage.haskell.org/package/ncurses-0.2.16
* Docs `UI.NCurses`: https://hackage.haskell.org/package/ncurses-0.2.16/docs/UI-NCurses.html
* Docs `UI.NCurses.Panel`: https://hackage.haskell.org/package/ncurses-0.2.16/docs/UI-NCurses-Panel.html
