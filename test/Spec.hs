module Main (main) where

--import Test.HUnit ()
import Test.Hspec

import GameBoard 
    ( createCompleteBoard
    , addOffset
    , displayBoard
    , displayFinishedBoard
    , revealLocation
    , getSquare
    , iterateNeighbors
    , incrementBombCount
    , createRow
    , revealMarking
    , createEmptyBoard
    , updateSquares
    , PlayerMarking(..)
    , BoardState(..)
    , Location(..)
    , Square(..)
    , Board
    )

-- | Helper Function to hasten testing
createDefaultSquare :: Location -> Square
createDefaultSquare loc = Square {location = loc
                                 , isMine = False
                                 , neighboringMines = 0
                                 , playerMarking = Untouched }

-- | Helper Function to hasten testing
createDefaultBomb :: Location -> Square
createDefaultBomb loc = Square {location = loc
                               , isMine = True
                               , neighboringMines = 0
                               , playerMarking = Untouched}

spec :: Spec
spec = do
  describe "addOffset" $ do
    it "adds Location 0 1 with Location 0 1" $
        addOffset (Location 0 1) (Location 0 1) `shouldBe` (Location 0 2)

    it "adds Location 0 0 with Location 0 1" $
        addOffset (Location 0 0) (Location 0 1) `shouldBe` (Location 0 1)

    it "adds Location 0 0 with Location 0 0" $
        addOffset (Location 0 0) (Location 0 0) `shouldBe` (Location 0 0)

    it "adds Location 0 -1 with Location 0 0" $
        addOffset (Location  0 (-1)) (Location 0 0) `shouldBe` (Location 0 (-1))

  describe "iterateNeighbors" $ do
    let width = 5
    let height = 5
    let centerLocation = (Location 2 2)
    let cornerLocation = (Location 0 0)
    let edgeLocation = (Location 0 2)

    it "returns a list of the 8 surrounding locations from the center" $
        (iterateNeighbors centerLocation width height) `shouldMatchList` 
            [(Location 1 1), (Location 1 2), (Location 1 3)
            ,(Location 2 1),                 (Location 2 3)
            ,(Location 3 1), (Location 3 2), (Location 3 3)]

    it "returns a list of the surrounding 3 locations from the corner location" $
        (iterateNeighbors cornerLocation width height) `shouldMatchList`
            [                (Location 1 0)
            ,(Location 0 1), (Location 1 1)]

    it "returns a list of the surrounding 5 locations from the edge location" $
        (iterateNeighbors edgeLocation width height)  `shouldMatchList`
            [(Location 0 1),                 (Location 0 3)
            ,(Location 1 1), (Location 1 2), (Location 1 3)]

    it "should not return any location that isn't immediately beside the middle" $
        (iterateNeighbors centerLocation width height) `shouldNotContain`
        [(Location 0 0), (Location 0 1), (Location 4 4)]
        
  describe "incrementBombCount" $ do
    let initBombSquare = Square { location = Location 0 0
                                , isMine = True
                                , neighboringMines = 0
                                , playerMarking = Untouched }
    let initSquare = Square {location = Location 0 0
                            , isMine = False
                            , neighboringMines = 0
                            , playerMarking = Untouched }
    let newSquare = Square { location = Location 0 0
                           , isMine = False
                           , neighboringMines = 1
                           , playerMarking = Untouched }

    it "returns the same square but with neighboringMines + 1" $
        incrementBombCount initSquare `shouldBe` newSquare

    it "doesn't increment neighboringMines of a bomb square" $
        incrementBombCount initBombSquare `shouldBe` initBombSquare

  describe "createRow" $ do
    it "should create a 5 long row Squares" $
        length (createRow 0 0 5 []) `shouldBe` 5 

    it "should create a 1 long row of None bomb Squares" $
        (createRow 0 0 1 []) `shouldMatchList` 
            [Square { location = Location 0 0
                   , isMine = False
                   , neighboringMines = 0
                   , playerMarking = Untouched}]

    it "should create a 1 long row of bomb Squares" $
        (createRow 0 0 2 [Location 0 0, Location 1 0]) `shouldMatchList`
            [Square { location = Location 0 0
                    , isMine = True
                    , neighboringMines = 0
                    , playerMarking = Untouched},
            Square  { location = Location 1 0
                    , isMine = True
                    , neighboringMines = 0
                    , playerMarking = Untouched}]

  describe "revealMarking" $ do
    let unvisitedMine  = Square { location = Location 0 0
                    , isMine = True
                    , neighboringMines = 0
                    , playerMarking = Untouched}
    let unvisitedSquare  = Square { location = Location 0 0
                    , isMine = False
                    , neighboringMines = 0
                    , playerMarking = Untouched}
    let visitedMine = Square { location = Location 0 0
                    , isMine = True
                    , neighboringMines = 0
                    , playerMarking = LostMine}
    let visitedSquare  = Square { location = Location 0 0
                    , isMine = False
                    , neighboringMines = 0
                    , playerMarking = Visited}

    it "should change an unvisited none bomb to visited" $
        (revealMarking unvisitedSquare) `shouldBe` visitedSquare

    it "should change an unvisited bomb to LostMine" $
        (revealMarking unvisitedMine) `shouldBe` visitedMine
    
  describe "createEmptyBoard" $ do
    it "should have 5 rows given a 5x5 board" $
        length (createEmptyBoard 0 0 5 5 []) `shouldBe` 5

    it "should contain 25 squares given a  5x5 " $
        length (concat (createEmptyBoard 0 0 5 5 [])) `shouldBe` 25

    it "should make a 2x2 board with default squares (i.e no bombs)" $
        (createEmptyBoard 0 0 2 2 []) `shouldMatchList` 
        [[createDefaultSquare $ Location 0 0, createDefaultSquare $ Location 1 0]
        ,[createDefaultSquare $ Location 0 1, createDefaultSquare $ Location 1 1]]

  describe "createCompleteBoard" $ do
    it "should give the same result of createEmptyBoard if there's no bombs" $
        createCompleteBoard 5 5 [] `shouldMatchList` createEmptyBoard 0 0 5 5 []

    it "should all have neighboringMines = 1 in a 3x3 if the bomb is in the middle" $
        createCompleteBoard 3 3 [Location 1 1] `shouldMatchList` [[
            incrementBombCount $ createDefaultSquare $ Location 0 0, 
            incrementBombCount $ createDefaultSquare $ Location 1 0,
            incrementBombCount $ createDefaultSquare $ Location 2 0],
           [incrementBombCount $ createDefaultSquare $ Location 0 1,
           createDefaultBomb $ Location 1 1,
            incrementBombCount $ createDefaultSquare $ Location 2 1],
           [incrementBombCount $ createDefaultSquare $ Location 0 2, 
            incrementBombCount $ createDefaultSquare $ Location 1 2,
            incrementBombCount $ createDefaultSquare $ Location 2 2]]

  describe "updateSquares" $ do 
    it "should update by incrementing bomb count of every square in a 2 x 2 board" $
        concat (updateSquares [Location 0 0, Location 0 1, Location 1 0, Location 1 1]
          (createCompleteBoard 2 2 []) incrementBombCount) `shouldMatchList` 
          [ incrementBombCount $ createDefaultSquare $ Location 0 0
          , incrementBombCount $ createDefaultSquare $ Location 0 1
          , incrementBombCount $ createDefaultSquare $ Location 1 0
          , incrementBombCount $ createDefaultSquare $ Location 1 1 ]
    
    it "should do nothing if no location list is provided" $ do
        let emptyBoard = (createCompleteBoard 5 5 [])
        updateSquares [] emptyBoard incrementBombCount `shouldMatchList` emptyBoard

  


main :: IO ()
main = hspec spec