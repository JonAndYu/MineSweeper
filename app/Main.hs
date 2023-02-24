module Main (main) where
import Data.List ()
import System.IO ()
import Data.Char ()
import System.Directory ()
import System.Random (randomRIO)

-------------------
-- Data Definitions
-------------------

-- A gameboard is made up of n x m Squares
--  * location = Index of the current square on the gameboard
--  * isMined = Has the player visited the square
--  * neighboringMines = Number of mines directly beside the square
--  * playerMarking = The state a player has interacted with the square
--      - Untouched - Player has not visited
--      - LostMine - Player visited and it was a mine
--      - Flagged - Player has not visited, but marked it with a flag
--      - Visited - Player has visited
--
data Square = Square 
    { location :: Location
    , isMine :: Bool
    , neighboringMines :: Int
    , playerMarking :: PlayerMarking
    } deriving (Show)

data Location = Location Int Int deriving (Eq, Show)

data PlayerMarking = Untouched | LostMine | Flagged | Visited deriving (Show)

-- A gameboard is defined as 
type Board = [[Square]]

-------------------
-- Board Creation
-------------------

-- We will hard code the number of bombs to be
-- floor(1/8 * boardLength * boardWidth)
-- 
-- generateBombAmount :: (RealFrac a) => a -> a -> Integer
-- generateBombAmount len width = floor (len * width * 0.125)

-- createBombLocation :: (Integral a) => Int -> Int -> a -> IO [(Int, Int)]
createBombLocation :: (Integral a) => Int -> Int -> a -> IO [Location]
createBombLocation len width amt = sequence [ randomLocation | _ <- [1..amt]]
    where
        randomLocation = do
            x <- randomRIO (0, len)
            y <- randomRIO (0, width)
            return (Location x y)

-- createRow :: Int -> Int -> Int -> [Location] -> [Square]
-- createRow xPos yPos width bombLocations = []

-- createBoard :: Int -> Int -> Int -> Int -> [Location] -> [[Square]]
-- createBoard xPos yPos length width bombLocations = []

main :: IO ()
main = do
    let x = 10
    tuples <- createBombLocation 8 10 10
    print tuples
