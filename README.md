# GameOfLife
This is a game of life implementation done in Haskell as part of an individual project in my Functional Programming class.

## How to run

After having GHCi installed one can run `stack runhaskell "Oblig.hs"` to run the script.

From there, the game can be played based on the rules of a file by providing "r *filename*". It also has a way of defining rules as you go, but I can´t remember how...

The "test.txt" file is just to demonstrate a scenario. It contains rules on the following form: *boardsize (how to survive as a cell and when a cell is born) coordinates of cells in the starting game as row col row col etc.*

When the board is displayed, type "quit" to quit the script, "l *generations* (l 5 for instance) to autorun for x number of *generations*". One can also skip generations by pressing *ENTER*.

Play with the rules in the file and figure out how it works!

