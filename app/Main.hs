{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.List (nub)
import System.IO (SeekMode (AbsoluteSeek))
import Data.Char ()
import System.Directory ()
import System.Random (randomRIO, randomRs, newStdGen)
import Data.Array
import Data.Time (TimeLocale(time12Fmt))

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

-- A gameboard is defined as 
type Board = [[Square]]

-------------------------
-- Board Creation Helpers
-------------------------

help :: (Int, Int) -> Location
help (x, y) = Location x y

{- | Creates a list of random Locations which comprises of (x, y) coordinates, 0 <= x < len and 0 <= y < width
TODO: https://github.com/JonAndYu/MineSweeper/issues/3
TLDR: Potentially generates the same location, especially for small boards, We need them to be n unique locations.
-}
-- createBombLocation :: (Integral a) => Int -> Int -> a -> [Location]
-- createBombLocation len width amt = sequence [randomLocation | x <- [1..amt]]
--   where
--     randomLocation = do
--         {-
--         [Every coordinate within the board]
--         rng a single coordinate,
--         remove the single coordinate from the list 
--         take another corodinate till you have what you need
--         -}
--         -- x <- randomRIO (0, len - 1)
--         -- y <- randomRIO (0, width - 1)
--         -- return (Location x y)
--         g <- newStdGen 
--         let list = [Location x y | x <- [0..len-1], y <- [0..width-1]] -- (0,0) (0,1) (0,2)... (n,n)
--         let x = nub $ (randomRs (0, width*len - 1) g :: [Int]) -- return an index
--         -- return (map (\i -> list !! i) x)
--         return list !! head x

randomIndex len width amt = do
    g <- newStdGen
    return . take amt . nub $ (randomRs (0, len * width - 1) g :: [Int])

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
modifyMarking :: Square -> PlayerMarking -> Square
modifyMarking (Square w x y z) marking = Square {location = w, isMine = x, neighboringMines = y, playerMarking = marking}

-- | Helper function to increment the bomb count at a given square.
incrementBombCount :: Square -> Square
incrementBombCount (Square w x y z) = Square {location = w, isMine = x, neighboringMines = (if x then 0 else 1) + y, playerMarking = z}

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

-- Creates a string that is pretty to print.
displayBoard :: Board -> String
displayBoard board = unlines $ map (unwords . map (show . getSquare)) board
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "(" ++ (if isMine then "M:" else "o:") ++ show neighboringMines ++ ")"
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "("++ show x ++ "," ++ show y++")"
  where 
    getSquare (Square (Location x y) isMine neighboringMines playerMarking) 
        | playerMarking == Untouched = " - "
        | playerMarking == Visited = " " ++ show neighboringMines ++ " "
        | playerMarking == Flagged = " F "
        | otherwise = " X " -- Bomb

-- playGame :: IO(String)
-- playGame :: do



main :: IO ()
main = do
    let boardWidth = 8
    let boardHeight = 8
    let bombAmount = 15
    let possible = [Location x y | x <- [0..boardWidth - 1], y <- [0..boardHeight - 1]]
    indices <- randomIndex boardWidth boardHeight 15
    let locations = map (\i -> possible !! i) indices
    let board = createEmptyBoard 0 0 boardWidth boardHeight locations
    let newBoard = updateSquares (iterateNeighbors (Location 0 1) boardWidth boardHeight ) board incrementBombCount
    print locations
    putStrLn ( displayBoard (createCompleteBoard boardWidth boardHeight locations) )