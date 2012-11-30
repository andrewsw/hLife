# hLife

This is a simple implementation of Conway's Game of Life. Mostly it's just me playing around with Haskell again after a while away. The original motivation is to develop some code that's a goal for teaching my kids some programming. I remember learning about and writing an implementation of Life in the mid-80s and hope my kids might be similarly fascinated by it. We'll see how that goes... Also, it's a chance to play with the Gloss library.

## Building

If you have a reasonably complete Haskell installation, you can probably get away with just simply building the main file as follows:

    ghc --make hLife.hs

This will produce hLife as an executable which gives a console version of Life complete with a simple menu.

Or, you can use the spiffy cabal file which probably needs some love, but works for me for the moment:

    cabal install --prefix=$SOMEWHERE --user

No guarantees I've done this right as I've not done it before...

## Running

The console version has a simple menu to drive its operation. To quit the Gloss version, try the escape key ;)

### Usage

    hLife [OPTION...]

* -s SIZE   --size=SIZE    board size as <width>x<height>
* -g        --gloss        use the Gloss graphical interface
* -f FILE   --file=FILE    read the supplied input file (will center in SIZE)

The file option is not supported yet, but will read a file of the following format:

    ! This is a comment
    ....O
    .....O
    ...OOO
    ! the above is a glider

The idea is that the input as read from the file will be centered in the specified board size. So if the above was read in with a specified height of 5, then there would be two complete empty lines, one above and one below the glider. Similarly for the width, the longest line will be used to determine the placement horizontally within the specified size.

## Future

Hopefully I'll implement some more features soon, like choosing the board size, or perhaps loading a file. We'll see. Also, I'd like to rewrite the code in Literate Haskell as a teaching tool, time and motivation permitting.

All comments welcome.



