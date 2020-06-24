import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List
import Control.Monad (return)
import GameBoard

applyMoves2 moves = foldl (\gs (m,c) -> fst (gamersTurn gs c m)) (Game createEmptyBoard Ongoing) (zip moves turnsSeq)
-- let res = mapM (\_ -> generate randomGameStatesWithManyMoves ) [1..1000]
randomGameStatesWithManyMoves = do
  ms <- shuffle $ concat (replicate 6 [0..6])
  let gs@(Game b s) = applyMoves2 ms
  return (b,s)


prop_gameEndsWhenMovesCanCoverWholeBoard = 
  forAll randomGameStatesWithManyMoves $ \(b, status)->
    status /= Ongoing


diskInLineHasAllPositionsFourTimes = null (elementsFromInLineIterator \\ fourTimesAllPositions) &&
  null (fourTimesAllPositions \\ elementsFromInLineIterator) @?= True
  where
    elementsFromInLineIterator = concat disksInALineIterator
    fourTimesAllPositions = concat (replicate 4 [(x,y)| x <- [0..bW-1], y <- [0..bH-1]])

applyMoves b' r' y' = addDisk (addDisk b' Red r') Yellow y'
createBoardForTest moves = foldl (\b (r,y) -> applyMoves b r y) createEmptyBoard (uncurry zip $ moves)


gameWithDiskInVerticalLineEndsWithWinner = do
  (moves, expectedResult) <- [
      (([1,1,1,1],[2,3,2,3]), RedWon),
      (([5,6,5,5], [1..4]), YellowWon),
      (([0,1,2,2,3,3,3], [1,2,3,3,5,5,5]), RedWon),
      ((concat (replicate 3 [0,1,4,5,2,3,6]),concat (replicate 3 [2,3,6,0,1,4,5])), Draw)
    ]
 
  return $ (getGameStatus (createBoardForTest moves) @?= expectedResult)

isProperMoveFollowsGameRules = do

  let moves = ((replicate 3 0), (replicate 3 0))
  let brd = createBoardForTest moves

  (move, expected) <- [
      (-1, False),
      (0, False),
      (1, True)
    ]
  
  return $ isProperMove brd move @?= expected


main :: IO ()
main = defaultMain tests

tests = [
    testGroup "functions operating on game board" ([
      testCase "goes through all positions four times" diskInLineHasAllPositionsFourTimes
      ] ++ (testCase "is proper move follows game rules" <$> isProperMoveFollowsGameRules)),
    testGroup "game result" $ testCase "game won when they put four discs in line" <$> gameWithDiskInVerticalLineEndsWithWinner,
    testGroup "game ends when enough moves" [testProperty "game ends" prop_gameEndsWhenMovesCanCoverWholeBoard]
  ]
