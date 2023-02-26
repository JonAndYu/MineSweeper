module Main (main) where
import Data.List (nub)
import Data.Char ()
import System.Directory ()
import System.Random (randomRs, newStdGen)
import Text.Read (readMaybe)
import qualified Control.Monad.IO.Class
import GameBoard
import RandomIndexGen

parseInput :: String -> Maybe (Int, Int)
parseInput input = case words input of
                     [a, b] -> case (readMaybe a, readMaybe b) of
                                 (Just x, Just y) -> Just (x, y)
                                 _                -> Nothing
                     _      -> Nothing

{- | Given the length and width of a game board, 
getUserInput obtains a valid location on the board.
A location is valid if:
    - It is unvisited
    - 0 <= x < width
    - 0 <= y < length
-}
getUserInput :: BoardState -> IO Location
getUserInput (BoardState board width len w x y i) = do
    putStrLn "Please enter in the form: x y"
    input <- getLine
    case parseInput input of
        Just (a, b) -> do
            if z == Visited
            then getUserInput (BoardState board width len w x y i)
            else return (Location a b)
            where Square _ _ _ z = getSquare board (Location a b)
        Nothing     -> getUserInput (BoardState board width len w x y i)

gameLoop :: BoardState -> IO BoardState
gameLoop boardState = do
    input <- getUserInput boardState
    let newState = revealLocation input boardState
    return newState

main :: IO BoardState
main = do
    let boardWidth = 8
    let boardHeight = 8
    let bombAmount = 15
    let possible = [Location x y | x <- [0..boardWidth - 1], y <- [0..boardHeight - 1]]
    indices <- randomIndex boardWidth boardHeight bombAmount
    let locations = map (\i -> possible !! i) indices
    let initGameState = createInitialGameState (createCompleteBoard boardWidth boardHeight locations) boardWidth boardHeight locations
    game initGameState

isFinalState :: BoardState -> Bool
isFinalState (BoardState _ _ _ _ lose nonBombs turns) = lose || (nonBombs == turns)

-- | True if player won, false if player hit bomb
winOrLose :: BoardState -> Bool
winOrLose (BoardState _ _ _ _ lose _ _) = not lose

winGame :: BoardState -> IO BoardState
winGame bs = do
    putStrLn "You Win!"
    putStrLn (displayFinishedBoard bs)
    return bs

lostGame :: BoardState -> IO BoardState
lostGame bs = do
    putStrLn "You lose!"
    putStrLn (displayFinishedBoard bs)
    return bs

game :: IO BoardState -> IO BoardState
game s = do
    state <- s
    if isFinalState state then (if winOrLose state then winGame else lostGame) state
    else
      do
        putStrLn (displayBoard state)
        game (gameLoop state)
