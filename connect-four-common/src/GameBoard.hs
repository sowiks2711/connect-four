{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : GameBoard
Description : Set of types and functions for simulating Connect Four game
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

Module implemented for Functional Programming in Haskell 2020 course at University of Warsaw.
Contains of data objects and functions that are used in http server and clients implementations
allowing for playing game of connect four.
-}
module GameBoard (
  Color(..)
  ,GameStatus(..)
  ,Board
  ,GameState(..)
  ,turnsSeq
  ,bH
  ,bW
  ,createEmptyBoard
  ,addDisk
  ,isProperMove
  ,disksInALineIterator
  ,getGameStatus
  ,checkIfDraw
  ,takeBoardElement
  ,runGamePlay
  ,gamersTurn 
  ,rowOfTopDisk
) where

import Data.Array.IArray (Array, array, bounds, (!), (//))
import Data.List (intercalate, foldl', find)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad.State.Strict

-- | Data type for distinguishing different players disks on game board
data Color = Red | Yellow deriving (Eq, Ord)

instance Show Color where
  show Red = "R"
  show Yellow = "Y"

-- | Function generating alternating sequence of player's colors
turnsSeq = Red:Yellow:turnsSeq

-- | Status of game
data GameStatus = Ongoing | RedWon | YellowWon | Draw deriving (Show, Eq)

-- | Type for storing dropped disks positions
-- each position in array represents dropped disk
-- with color indicating given player or absence 
-- of such in a given position
newtype Board = CF (Array (Int, Int) (Maybe Color))

-- | Max number of disks stacked in one column on a game board
bH = 6
-- | Max number of disks that can be layed down in a horizontal line on a game board
bW = 7


-- | Record for storing board state and status
data GameState = Game {
  gameBoard :: Board,
  status :: GameStatus
}


instance Show Board where
  show board = intercalate "\n" [[ positionToChar (y,x) | x <- [0..bW-1]] | y <- [0..bH-1]]
    where
      positionToChar (x,y) = case takeBoardElement board x y of
        Nothing -> 'O'
        (Just c) -> head . show $ c


-- | Creates empty board
createEmptyBoard :: Board
createEmptyBoard =  CF (array ((0, 0), (5, 6)) [((r, c), Nothing) | r <- [0..5], c <- [0..6]])

-- | Adds player's disk to a given board column
addDisk :: Board -- ^ Board for which we are dropping new disk
        -> Color -- ^ Disk color
        -> Int   -- ^ Board column (0-6)
        -> Board -- ^ New Board resulting from dropping new disk
addDisk b@(CF board) col column = CF (board // [(position, Just col)])
  where
    position = (rowOfTopDisk b column, column)

-- | Returns row where the next disk can be placed counting from the top beginning from 0
rowOfTopDisk :: Board
             -> Int
             -> Int
rowOfTopDisk board column =
  if isJust firstDropped then fromJust firstDropped - 1 else bH - 1
  where
    firstDropped = find (\row -> isJust (takeBoardElement board row column)) [0..bH-1]

-- | Checks if dropping disk in given position is possible
isProperMove :: Board -- ^ Given board configuration
             -> Int   -- ^ Column for dropping new disk
             -> Bool  -- ^ Answers if given move is legal
isProperMove board column = column < bW && column >= 0 && not columnIsFull
  where
    columnIsFull = isJust (takeBoardElement board 0 column)

-- | Sequence of positions for each line
disksInALineIterator =
 -- vertical lines
 [[(x,y) | y <- [0..bH-1]] | x <- [0..bW-1]] ++
 -- horizontal
 [[(x,y) | x <- [0..bW-1]] | y <- [0..bH-1]] ++
 -- right diagonals
 [[(x+y,y) | y <- [0..bH-1], x+y<bW] | x <- [0..bW-1]] ++
 [[(y,y+z) | y <- [0..bH-1], y<bW, y+z<bH] | z <- [1..bH-1]] ++
 -- left diagonals
 [[(x-y,y) | y <- [0..bH-1], x-y>=0] | x <- [bW-1, bW-2..0]] ++
 [[(bW-1-y,y+z) | y <- [0..bH-1], bW-1-y>=0, y+z<bH] | z <- [1..bH-1]]


-- | Retrieves game status for given moves configuration
getGameStatus :: Board -> GameStatus
getGameStatus board = checkIfDraw board $ foldl' updateStatus Ongoing (map (calculateMaxInLineDiscs board) disksInALineIterator)
  where
    updateStatus Ongoing (r,y)
      | r >= 4 = RedWon
      | y >= 4 = YellowWon
      | otherwise = Ongoing
    updateStatus s (r,y) = s

-- | Checks if game ended with a draw
checkIfDraw :: Board -> GameStatus -> GameStatus
checkIfDraw b Ongoing = maybe Ongoing (const Draw) $
    sequence [takeBoardElement b y x | x <- [0..bW-1], y <- [0..bH-1]]
checkIfDraw _ s = s


-- | Retrieves Just disk color or Nothing from a given board position
takeBoardElement :: Board       -- ^ Board configuration
                 -> Int         -- ^ Board row
                 -> Int         -- ^ Board column
                 -> Maybe Color -- ^ Just Color of a disk or Nothing
takeBoardElement (CF board) i j
  | i < minRow || i > maxRow || j < minCol || j > maxCol = Nothing
  | otherwise = board ! (i, j)
  where ((minRow, minCol), (maxRow, maxCol)) = bounds board


calculateMaxInLineDiscs board posInLine = (startColorLoop Red, startColorLoop Yellow)
  where
    startColorLoop c = calcDiscsLoop board c posInLine (0,0)

calcDiscsLoop board c ((i,j):xs) (maxAgg, agg) = calcDiscsLoop board c xs (if maxAgg < newAgg then newAgg else maxAgg, newAgg)
  where
    newAgg = case takeBoardElement' j i of
      Nothing -> 0
      Just c' -> if c == c' then agg + 1 else 0
    takeBoardElement' = takeBoardElement board
calcDiscsLoop _ _ [] (maxAgg,_) = maxAgg

type StGameIO a = StateT GameState IO a


runGamePlay :: IO ()
runGamePlay = do
  s <- evalStateT (gamePlay Red) (Game createEmptyBoard Ongoing)
  print s

gamePlay :: Color -> StGameIO GameStatus
gamePlay c = do
  move <- lift (read <$> getLine :: IO Int)
  gs <- get
  let (gms@(Game b s), isSuccess) = gamersTurn gs c move
  put gms
  lift . print $ (b, isSuccess)
  case (s,isSuccess) of
    (Ongoing, True) -> gamePlay (if c == Red then Yellow else Red)
    (Ongoing, False) -> gamePlay c
    _ -> return s

-- | Applies user move to game state
gamersTurn :: GameState         -- ^ State of game
           -> Color             -- ^ Color of disc being dropped indicating player
           -> Int               -- ^ Column on board where disk is being dropped
           -> (GameState, Bool) -- ^ Resulting game state and flag indicating if the move was appropriate
gamersTurn gs@(Game b s) c move =
  if isProperMove b move && s == Ongoing
    then
      (addMove move c gs, True)
    else
      (gs, False)

-- | Adds players next move to game state
addMove :: Int -> Color -> GameState -> GameState
addMove m c (Game b s) = Game newBoard (getGameStatus newBoard)
  where
    newBoard = addDisk b c m

