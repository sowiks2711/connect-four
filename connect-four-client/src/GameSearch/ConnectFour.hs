{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

{-|
Module      : GameSearch.ConnectFour
Description : Spec class methods implementation for connect four game state
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

-}
module GameSearch.ConnectFour (start, State(..), Drop(..)) where

import Data.Array.IArray (Array, array, bounds, (!), (//))
import Data.List         (find)
import Data.Maybe        (fromJust, isJust)
import GameSearch.Core   (Spec (..))
import GameBoard

newtype Drop = Drop Int deriving (Eq, Ord, Show)

data State = State
    { grid   :: Board
    , turn   :: Color
    , winner :: Maybe Color
    }

instance Show State where
    show = show . grid

instance Spec State Drop Color where
    -- | Returns positions where the columns are not filled up.
    actions s
        | isJust (winner s) = []
        | otherwise         = availDrops (grid s)

    -- | Returns a payout of 1 if we won, 0 if we lost.
    payouts s =
        let payout = payoutOf (winner s) in [(Red, payout), (Yellow, -payout)]

    player = turn

    -- | Applies action a to board b.
    apply (Drop c) s =
        State
        { grid   = newGrid
        , turn   = if turn s == Red then Yellow else Red
        , winner = if winningMove then Just (turn s) else Nothing
        } where
            dropPos     = (droppedRow (grid s) c, c)
            newGrid     = addDisk (grid s) (turn s) c
            winningMove = isWinningMove newGrid dropPos (turn s)

-- | Return the starting state.
start :: State
start = State
    { grid = createEmptyBoard
    , turn = Red
    , winner = Nothing
    }

-- | Returns the positions where the columns aren't filled up.
availDrops :: Board -> [(Double, Drop)]
availDrops grid =
    map (\pos -> (1.0, Drop pos)) $
    filter (\col -> isProperMove grid col) [0..6]

-- | Returns the playout of a possible winner.
payoutOf :: Maybe Color -> Double
payoutOf (Just Red) = 1.0
payoutOf (Just Yellow) = -1.0
payoutOf Nothing    = 0.0

-- | Get the row that the piece settles in if we drop it at that column.
droppedRow :: Board -> Int -> Int
droppedRow board col = rowOfTopDisk board col

-- | An O(1) win check for connect 4 around a piece that is independent of board
-- size.
isWinningMove :: Board -> (Int, Int) -> Color -> Bool
isWinningMove board (r, c) player =
    any (all (\(i, j) -> takeBoardElement board (r + i) (c + j) == Just player))
        [ [(0, 0),  (0, 1),  (0, 2),  (0, 3)]
        , [(0, -1), (0, 0),  (0, 1),  (0, 2)]
        , [(0, -2), (0, -1), (0, 0),  (0, 1)]
        , [(0, -3), (0, -2), (0, -1), (0, 0)]

        , [(0, 0),  (1, 0),  (2, 0),  (3, 0)]
        , [(-1, 0), (0, 0),  (1, 0),  (2, 0)]
        , [(-2, 0), (-1, 0), (0, 0),  (1, 0)]
        , [(-3, 0), (-2, 0), (-1, 0), (0, 0)]

        , [(0, 0),   (1, 1),   (2, 2),   (3, 3)]
        , [(-1, -1), (0, 0),   (1, 1),   (2, 2)]
        , [(-2, -2), (-1, -1), (0, 0),   (1, 1)]
        , [(-3, -3), (-2, -2), (-1, -1), (0, 0)]

        , [(0, 0),  (-1, 1), (-2, 2), (-3, 3)]
        , [(1, -1), (0, 0),  (-1, 1), (-2, 2)]
        , [(2, -2), (1, -1), (0, 0),  (-1, 1)]
        , [(3, -3), (2, -2), (1, -1), (0, 0)]
        ]
