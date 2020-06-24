{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Handlers
Description : Set of functions that handles requests for connect four game server API
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

Module implemented for Functional Programming in Haskell 2020 course at University of Warsaw.
-}
module Handlers (
  regHandler
  ,makeMoveHandler
  ,getOpponentMove
  ,getGameResult
  ,ServerState(..)
  ,AppM
) where

import           Control.Monad               (when, unless)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Monad.STM           (atomically, STM)
import           System.Random
import           Data.Text                   (Text, unpack)
import           Data.UUID
import           Data.Maybe                  (isNothing)
import           Servant
import           DataContract
import           GameBoard

-- | Monad transformer combining servant monads with custom server state
type AppM = ReaderT ServerState Handler

-- | Data type for storying game server state, game and players
data ServerState = SState {
  gameState :: TVar GameState,
  playerOne :: TVar (Maybe String), --Always Red
  playerTwo :: TVar (Maybe String), --Always Yellow
  lastMove :: TVar (Maybe Int),
  nextPlayer :: TVar Color,
  rndGen :: TVar StdGen
}
readServerState :: AppM (GameState, Maybe String, Maybe String, Maybe Int, Color, StdGen)
readServerState = do
  SState{gameState = gs, playerOne = p1, playerTwo = p2, lastMove = lm, nextPlayer = np, rndGen = gn } <- ask
  liftIO $ atomically $ do
    gs' <- readTVar gs
    p1' <- readTVar p1
    p2' <- readTVar p2
    lm' <- readTVar lm
    np' <- readTVar np
    rnd' <- readTVar gn
    return (gs', p1', p2', lm', np', rnd')

-- | Handles requests for registration for new game
regHandler :: AppM GameRegistration
regHandler = do
  (gs@(Game _ s), _, _, _, _, _) <- readServerState
  when (s /= GameBoard.Ongoing) resetGame
  (gs@(Game _ s), p1, p2, _, _, _) <- readServerState
  case (p1, p2) of
    (Nothing, Nothing) -> register fst YourTurn
    (_, Nothing) -> register snd WaitForTurn
    (_, _) -> throwError err409 { errBody = "Game is already on. Wait for current game to end and try again later." }
  where
    resetGame = do
      SState{gameState = g, playerOne = p1, playerTwo = p2, nextPlayer = np} <- ask
      liftIO $ atomically $ do
        writeTVar g (Game createEmptyBoard Ongoing)
        writeTVar p1 Nothing
        writeTVar p2 Nothing
        writeTVar np Red
        return ()

register selector turn = do
  SState{rndGen = rg, playerOne = p1', playerTwo = p2'} <- ask
  let p = selector (p1', p2')
  playerId <- getRandomId p rg
  liftIO $ print playerId
  liftIO $ print $ "Game state:" ++ show playerId
  return (Registration playerId turn)
  where
    getRandomId p rg = liftIO $ atomically $ do
      rg' <- readTVar rg
      let (val, rg'') = random rg'
      writeTVar p (Just (toString val))
      writeTVar rg rg''
      return $ toString val

-- | Handles requests for making a next move in game for players
makeMoveHandler :: Maybe Text -> Int -> AppM ()
makeMoveHandler s m = do
  SState{gameState = gs, rndGen = gn, playerOne = p1, playerTwo = p2, lastMove = lm, nextPlayer = c} <- ask
  (userId, color) <- liftIO $ atomically $ do
    nextPlayer <- readTVar c
    getUserId p1 p2 nextPlayer
  liftIO $ print userId
  when (userId /= (unpack <$> s)) (throwError err409{errBody = "Please wait for your turn."})
  gs' <- liftIO $ atomically $ readTVar gs
  let (newGameState@(Game b s), isSuccess) = gamersTurn gs' color m
  unless isSuccess $ throwError err409{errBody = "Illegal move"}
  liftIO $ print b >> print s >> print isSuccess
  _ <- liftIO $ atomically $ do
    writeTVar gs newGameState
    writeTVar c (if color == Red then Yellow else Red)
    writeTVar lm (Just m)

  return ()
  where
    getUserId p1' p2' c' =
      flip (,) c' <$> readTVar (colorToUser c' p1' p2')

colorToUser :: Color -> a -> a -> a
colorToUser c p1 p2 =
  case c of
    Red -> p1
    _ -> p2

-- | Method for querying for opponent's move
getOpponentMove :: Maybe Text -> AppM OpponentMove
getOpponentMove uId = do
  (gs@(Game _ s), p1, p2, lm, np, _) <- readServerState
  checkIfRequestFromPlayer uId p1 p2
  let responseBuilder = OpponentMove lm
  let uId' = unpack <$> uId
  case ((uId' == p1 && np == Red) || (uId' == p2 && np /= Red), s == Ongoing || isNothing p1 || isNothing p2) of
    (True, True)  -> return $ responseBuilder YourTurn
    (False, True) -> return $ responseBuilder WaitForTurn
    (_, _)        -> return $ responseBuilder GameOver

-- | Allows players to get the game result after the game ended
getGameResult :: Maybe Text -> AppM GameResult
getGameResult uId = do
  SState{gameState = gs, playerOne = p1, playerTwo = p2} <- ask
  (gs@(Game b s), p1', p2') <- liftIO $ atomically $ do
    p1' <- readTVar p1
    p2' <- readTVar p2
    gs' <- readTVar gs
    return (gs', p1', p2')

  checkIfRequestFromPlayer  uId p1' p2'

  case s of
    GameBoard.RedWon | p1' == (unpack <$> uId)    -> return DataContract.YouWon
    GameBoard.YellowWon | p2' == (unpack <$> uId) -> return DataContract.YouWon
    GameBoard.Draw                                -> return DataContract.Draw
    GameBoard.Ongoing                             -> throwError err409{errBody="Game is still ongoing"}
    _                                             -> return DataContract.YouLost

-- | checks if request contains id of player registered current  for game
checkIfRequestFromPlayer :: Maybe Text -> Maybe String -> Maybe String -> AppM ()
checkIfRequestFromPlayer uId p1 p2=
  unless (foldl (\agg x -> (unpack <$> uId) == x || agg) False [p1, p2]) (throwError err401{errBody="You are not a player in current game"})
