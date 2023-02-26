module GameBoard 
    ( createCompleteBoard
    , createInitialGameState
    , displayBoard
    , displayFinishedBoard
    , revealLocation
    , getSquare
    , PlayerMarking(..)
    , BoardState(..)
    , Location(..)
    , Square(..)
    ) where
import Data.Char ()

-------------------
-- Data Definitions
-------------------

{- | A gameboard is made up of n x m Squares
    location = Index of the current square on the gameboard
    isMined = Has the player visited the square
    neighboringMines = Number of mines directly beside the square
    playerMarking = The state a player has interacted with the square
        Untouched - Player has not visited
        LostMine - Player visited and it was a mine
        Flagged - Player has not visited, but marked it with a flag
        Visited - Player has visited
-}
data Square = Square
    { location :: Location
    , isMine :: Bool
    , neighboringMines :: Int
    , playerMarking :: PlayerMarking
    } deriving (Show)

data Location = Location Int Int deriving (Eq, Show)

data PlayerMarking = Untouched | LostMine | Flagged | Visited deriving (Eq, Show)

data BoardState = BoardState
    { gameBoard :: Board
    , width :: Int
    , len :: Int
    , bombLocations :: [Location]
    , visitedBomb :: Bool
    , avaliableNoneBombSpaces :: Int
    , visitedSpaces :: Int
    }

-- A gameboard is defined as 
type Board = [[Square]]

-------------------------
-- Board Creation Helpers
-------------------------

-- | A mask of locations of all cardinal + ordinal directions from a given point, not including itself.
offsets :: [Location]
offsets = [Location x y | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addOffset :: Location -> Location -> Location
addOffset (Location x1 y1) (Location x2 y2) = Location (x1 + x2) (y1 + y2)

{- | Returns a list of locations around a given point using the offsets mask and addOffset function.
Includes boundary checks to ensure that the neighbors are within the area of the Board. 
-}
iterateNeighbors :: Location -> Int -> Int -> [Location]
iterateNeighbors selectedLocation width height = [Location x y |
    Location x y <- map (addOffset selectedLocation) offsets,
    x >= 0 && x < width && y >= 0 && y < height
    ]

-------------------
-- Board Updates
-------------------

-- | Helper function to access a specfic square on a board
getSquare :: Board -> Location -> Square
getSquare board (Location x y) = board !! y !! x

-- | Helper function to modify a squares PlayerMarking
revealMarking :: Square -> Square
revealMarking (Square w x y z) = Square {location = w, isMine = x, neighboringMines = y, playerMarking = if x then LostMine else Visited }

-- | Helper function to increment the bomb count at a given square.
incrementBombCount :: Square -> Square
incrementBombCount (Square w x y z) = Square {location = w, isMine = x, neighboringMines = (if x then 0 else 1) + y, playerMarking = z}

revealLocation :: Location -> BoardState -> BoardState
revealLocation location (BoardState g w l locations visitBomb avaNoneBombs visitedCount) = BoardState
    { gameBoard = updateSquares [location] g revealMarking
    , width = w
    , len = l
    , bombLocations = locations
    , visitedBomb = (location `elem` locations) || visitBomb
    , avaliableNoneBombSpaces = avaNoneBombs
    , visitedSpaces = visitedCount + 1
    }


{- | Abstracted function that takes in:
        [Location]          - A list of locations that need to be modified.
        Board               - The current board that needs to be changed.
        (Square -> Square)  - A function that modifies a Square.
    And returns a new board that contains the changes. Because lists are immutable every time this function runs you're creating a new
    Board.
-}
updateSquares :: [Location] -> Board -> (Square -> Square) -> Board
updateSquares lst oldBoard f = [[if Location c r `elem` lst then f x else x | (c, x) <- zip [0..] row] | (r, row) <- zip [0..] oldBoard]

-------------------
-- Board Creation
-------------------

{- | Recursively creates a single row of the board. 
    Populates the row with squares of default values:
        Int         - The current column of the square.
        Int         - The current row of the square. (Never changes)
        Int         - The maximum number of columns in a row.
        [Location]  - A list of locations that contain a bomb.
    By default Squares are unvisited, have 0 neighboringMines, and are placed at the current (xPos, yPos) location.
    This function will check to if the current location is a bombLocation, if so it'll set the square's isMine to be True.
-}
createRow :: Int -> Int -> Int -> [Location] -> [Square]
createRow xPos yPos width bombLocations
    | Location xPos yPos `elem` bombLocations && xPos < width =
        Square { location = Location xPos yPos, isMine = True , neighboringMines = 0, playerMarking = Untouched }
        : createRow (xPos + 1) yPos width bombLocations
    | xPos < width = Square { location = Location xPos yPos, isMine = False, neighboringMines = 0, playerMarking = Untouched } : createRow (xPos + 1) yPos width bombLocations
    | otherwise = []

{- | Recursively creates the empty board with the help of createRow.
    Populates the Board with [squares] (rows) of default values:
        Int         - The current column of the square. (Never changes)
        Int         - The current row of the square.
        Int         - The maximum number of rows in the board.
        [Location]  - A list of locations that contain a bomb.
-}
createEmptyBoard :: Int -> Int -> Int -> Int -> [Location] -> Board
createEmptyBoard xPos yPos width len bombLocations
    | yPos < len = createRow xPos yPos width bombLocations : createEmptyBoard xPos (yPos + 1) len width bombLocations
    | otherwise = []

-- | Given an empty board returned by createEmptyBoard, this function will increment every non bomb square by the number of bombs surrounding it.
-- | Pseudo Type: foldr ([Location] -> Board -> Board) -> Initial Board -> [[Location]] -> Final Board  
createCompleteBoard :: Int -> Int -> [Location] -> Board
createCompleteBoard width len bombLocations = foldr (\locations accBoard -> updateSquares locations accBoard incrementBombCount) (createEmptyBoard 0 0 width len bombLocations) [iterateNeighbors x width len | x <- bombLocations]

createInitialGameState :: Board -> Int -> Int -> [Location] -> IO BoardState
createInitialGameState g w l bombs = do
    putStrLn ("Creating board with a width of " ++ show w ++ " and a length of " ++ show l ++ ". There are " ++ show (length bombs) ++ " on the board.")
    return BoardState { gameBoard = g, width = w, len = l, bombLocations = bombs, visitedBomb = False, avaliableNoneBombSpaces = w * l - length bombs, visitedSpaces = 0 }

-- Creates a string that is pretty to print.
displayBoard :: BoardState -> String
displayBoard (BoardState g _ _ _ _ _ _) = unlines $ map (unwords . map (show . getSquares)) g
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "(" ++ (if isMine then "M:" else "o:") ++ show neighboringMines ++ ")"
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "("++ show x ++ "," ++ show y++")"
  where
    getSquares (Square (Location _ _) _ neighboringMines playerMarking)
        | playerMarking == Untouched = " - "
        | playerMarking == Visited = " " ++ show neighboringMines ++ " "
        | playerMarking == Flagged = " F "
        | otherwise = " X " -- Bomb

displayFinishedBoard :: BoardState ->  String
displayFinishedBoard (BoardState g _ _ _ _ _ _) = unlines $ map (unwords . map (show . getSquares)) g
    where getSquares (Square _ isMine neighboringMines _) = " " ++ (if isMine then "X" else show neighboringMines ) ++ " "
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "("++ show x ++ "," ++ show y++")"