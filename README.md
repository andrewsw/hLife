# hLife

This is a simple implementation of Conway's Game of Life. Mostly it's just me playing around with Haskell again after a while away. The original motivation is to develop some code that's a goal for teaching my kids some programming. I remember learning about and writing an implementation of Life in the mid-80s and hope my kids might be similarly fascinated by it. We'll see how that goes... Also, it's a chance to play with the Gloss library.

## Building

If you have a reasonably complete Haskell installation, you can probably get away with just simply building the main file as follows:

    ghc --make hLife.hs

This will produce hLife as an executable which gives a console version of Life complete with a simple menu.

If you want to run the Gloss version, at this point you have to compile it separately as follows, which will need at least the Gloss package, maybe more:

    ghc --make LifeGloss.hs -main-is LifeGloss

and then run the resulting binary.

Or, you can use the spiffy cabal file which probably needs some love, but works for me for the moment:

    cabal install --prefix=$SOMEWHERE --user

No guarantees I've done this right as I've not done it before...

## Running

The console version has a simple menu to drive its operation. Both version currently only support running a randomly generated 50x50 board. To quit the Gloss version, try the escape key ;)

## Future

Hopefully I'll implement some more features soon, like choosing the board size, or perhaps loading a file. We'll see. Also, I'd like to rewrite the code in Literate Haskell as a teaching tool, time and motivation permitting.

All comments welcome.



