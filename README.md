# MineSweeper

My collaborator and I intend to explore the viability of Haskell as a general programming language through the developement of a simple timeless game. Due to the limited amount of time, not all the functionality of game could be implemented, but an analysis of their potential integration into the game will be included near the end of the `README` document.

Minesweeper is a classic single-player computer game that has been popular since its introduction in the early 1990s. The game is played on a n x n grid that represents a minefield, where each square of the grid can either be blank or contain a hidden mine. The objective of the game is to uncover all the squares on the grid without detonating any of the hidden mines.

At the start of the game, the player is presented with a blank grid, and must begin by selecting a square on the command line to reveal its contents. If the square contains a mine, the game is over and the player loses. If the square is blank, then it will reveal a number indicating how many mines are in the adjacent squares. The player can then use this information to make educated guesses about where the remaining mines might be, and continue to reveal additional squares until all the non-mine squares have been uncovered.
> The above 2 paragraphs were entirely written with ChatGPT

___
## Instructions to run game (Assumes you have stack, ghci, ghc):
* In the `main` function of `app/Main.hs` modify the values `n` and `bombAmount` to change the difficulty. Save the file.
> Note: $\text{bombAmount} < n^2$
* Build the project using `stack build`
* Run the game using `stack exec MineSweeper-exe`
    - The above commands can be chained together: `stack build && clear && stack exec MineSweeper-exe`
* To run associated tests use `stack test`

## Version Specific Instructions:

Upon starting the game, you will be shown basic information about the current game board, such as size and number of bombs in the map. The board was set up such that the upper left corner corresponds to $(0,0)$ and the bottom right corner corresponds to $(n-1, n-1)$.

|       | **0** | **1** | **2** | **3** | **4** |
|-------|-------|-------|-------|-------|-------|
| **0** | 0 0   | 1 0   | 2 0   | 3 0   | 4 0   |
| **1** | 0 1   | 1 1   | 2 1   | 3 1   | 4 1   |
| **2** | 0 2   | 1 2   | 2 2   | 3 2   | 4 2   |
| **3** | 0 3   | 1 3   | 2 3   | 3 3   | 4 3   |
| **4** | 0 4   | 1 4   | 2 4   | 3 4   | 4 4   |

**5 x 5 Example** : Each cell contains the user input necessary to access the square

* Every input should consist of a valid "col row" pair
    * A pair is valid if $0 \leq col < n$ and $0 \leq row < n$
* Checks were included to ensure that the user could not input invalid pairs. 

The program will terminate and notify the player if they uncover all the bombs on the board, resulting in a win. Alternatively, if the player uncovers a single bomb, they will lose and the program will also terminate. In both cases, the player must run `stack exec MineSweeper-exe` to restart.
___
## Improvements:

The following features were not implemented due to time constraint. However we included useful ideas incase the reader wants to improve on the program.

More robust system of selecting board size and bomb amount - Currently you need to directly modify the code to play the game with different attributes. The end result would be a compiled program that takes a list of arguments when ran which will modify the program.

* `System.Environment.getArgs` returns the arguments passed into your program as a list of strings.
* The developer would then need to parse the arguments to ensure they contain a numeric size and bomb amount.
* There are no checks to make sure $\text{bomb amount} > n^2$ so any problematic amount will crash the program.

Dynamically created bomb locations - At the moment, the program initializes the bomb location at the start of the game irrespective of where the player first clicks. This potentially causes the player to lose on their first turn. In the spirit of the original `Minesweeper`... the first click/coordinate should dictate where the rest of the bombs are placed.

Better display method - Potential solutions include a graphics library like `Gloss` or a pretty printer library. Much of the game logic exists as pure functions in `src/GameBoard.hs` so regardless of the method, it should be easier to remake the user interface.

Implement chording/zero-start - As iteratively explained [here](https://www.reddit.com/r/Minesweeper/comments/v481jm/i_want_to_know_how_the_tiles_open_up_when_clicked/), either method will cause multiple squares to be revealed if they do not exist near a bomb.

Replace the immuteable `Board`s with an immuteable array - While it doesn't seem to be in the spirit of functional programming. Whenever a user reveals a cell, a completely new board must be created just to modify a single existing cell. According to the [Haskell array wiki](https://wiki.haskell.org/Arrays), there exists arrays that story information on mutable arrays. It's potential can be explored in this project.
___

## Key Takeaways:

Higher order functions, functions that can be passed into other functions as arguments/returns functions, enabled us to more easy write generic code which can be reused in different parts of our program.

For example, `src/GameBoard.hs`'s `updateSquares` method came from our realization that iterating through the board to increment bomb counts (for board initialization) has an end result extremely similar to iterating through the board to reveal the marking whenever a player inputs a (row col) pair. The two functions `revealMarking` and `incrementBombCount` allowed us to do both these using the generic function `updateSquares`. Similar idea to out Board visualizers

Haskell's functional programming and our insistance to be as functional as possible my limiting the amount of `do/where/let` keywords had allowed us to discover the benefits of modularity. By writing small digestable functions out of necessaity, we avoided the common pitfall of many procedural programming with long functions. Our pure functions were as small we could make them so each non-generic function did a singular task.

Using Haskell's `HSpec` library was an easy experience because of functional programming encourages zero side effect functions. We were able to mock up individual components, such as `Squares`, `Locations`, and `Boards` and test them without having to worry about our function modifying some global object like you might have when testing classes in object oriented programming.

We wish we could tell you using Haskell was all sunshine and rainbows, but we can't...

In the spirit of Haskell, everything we used, including the data structure is immuteable. Unfortunately, for Minesweeper, this means that every time a user reveals a single new square, a completely new board must be created. For a small board size such as `n = 5` the performance hit is negligible, but as n increases the downsides will become apparent. While it may be true developers can optimize this problem by caching uneffected rows, the workarounds will never be as easy as reassignment in non-functional programming languages.


___
## Attributions:
The idea of `createRow` and `createBoard` came from a previous CPSC 312 Minesweeper project.
> https://github.com/ydai94/cs312_project

We got the idea of creating the data types from this code review by Franky.
> https://codereview.stackexchange.com/questions/264429/minesweeper-in-haskell
