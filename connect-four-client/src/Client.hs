{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Client
Description : Interactive cli and ai clients for connect four game server on http
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

Module implemented for Functional Programming in Haskell 2020 course at University of Warsaw.
-}
module Client
    ( clientConsole,
    clientAI
    ) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           Data.Maybe (fromMaybe, fromJust)
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.Types.Status (status409)
import           Servant.API
import           Servant.Client
import           Messages
import           GameBoard
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict (StateT, runStateT, get, modify)
import           Control.Monad.Trans.Class
import           Control.Monad.Error.Class (catchError, throwError)
import           Control.Concurrent     (threadDelay)
import           Text.Read              (readMaybe)
import           GameSearch             (Node, apply, bestAction, empty, timedMCTS,
                                        child)
import           GameSearch.ConnectFour (State, Drop (..), start, winner)
import qualified GameSearch.ConnectFour as GS
import           System.Random          (getStdGen)

initializeGameState = Game createEmptyBoard Ongoing


-- | Function that connects to connect four game server, enrolles for next game and provides console
-- interactive interface for conducing game with another connected player
clientConsole :: IO ()
clientConsole  = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (runStateT (Client.gamePlay getAndPostUserMove Registration) (State Nothing start GameSearch.empty)) (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
    handleGamePlayResult res

handleGamePlayResult res =
    case res of
        Left (FailureResponse _ response)       -> print response >> print (responseBody response)
        Right (reg, State _ gs _) -> print reg >> print gs >> print (winner gs)
        Left err                                -> putStrLn $ "Error:" ++ show err


-- | Function that connects to connect four game server, enrolles for next game and conducts game
-- using MCTS algorithm for providing next moves
clientAI :: IO ()
clientAI  = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (runStateT (Client.gamePlay getAndPostAIMove Registration) (State Nothing start GameSearch.empty)) (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
    handleGamePlayResult res

-- | Servant type-level API, generated from the Swagger spec for ConnectFour.
type ConnectFourAPI
    =    "game" :> "move" :> Header "userId" Text :> Verb 'GET 200 '[JSON] OpponentMove -- 'gameMoveGet' route
    :<|> "game" :> "move" :> Header "userId" Text :> Capture "m" Int  :> Verb 'POST 200 '[JSON] () -- 'gameMoveMPost' route
    :<|> "game" :> "register" :> Verb 'POST 200 '[JSON] GameRegistration -- 'gameRegisterPost' route
    :<|> "game" :> "result" :> Header "userId" Text :> Verb 'GET 200 '[JSON] GameResult -- 'gameResultGet' route


getOpponentMove :: Maybe Text -> ClientM OpponentMove

makeMove :: Maybe Text -> Int -> ClientM ()

registerGame :: ClientM GameRegistration

getGameResult :: Maybe Text -> ClientM GameResult

api :: Proxy ConnectFourAPI
api = Proxy

getOpponentMove :<|> makeMove :<|> registerGame :<|> getGameResult = client api

data GamePart = Registration | QueryingOpponentMove | MakingMove | GameOutcome
data ClientState = State (Maybe Text) GS.State (Node Drop Color)

type ClientStateM a =  StateT ClientState ClientM a

gamePlay :: ClientStateM Int -> GamePart -> ClientStateM ()
gamePlay moveProvider gamePart = do
    (State _ gs _) <- get
    liftIO $ print gs >> putStrLn "0123456"
    nextGamePart <- getNextPart gamePart
    case nextGamePart of
        (Just np) -> Client.gamePlay moveProvider np
        _       -> return ()
  where
    getNextPart gamePart =
        case gamePart of
            Registration         -> registerPlayerForGame
            QueryingOpponentMove -> queryForOpponentMove
            MakingMove           -> getPostUpdateUserMove moveProvider 
            GameOutcome          -> getGameOutcome

getGameOutcome :: ClientStateM (Maybe GamePart)
getGameOutcome = do
    (State playerId _ _) <- get
    res <- lift . getGameResult $ playerId
    liftIO $ print res
    return Nothing

getAndPostAIMove :: ClientStateM Int
getAndPostAIMove = do
    (State playerId mctsState node) <- get
    rand      <- liftIO getStdGen
    finalNode <- liftIO $ timedMCTS 5 rand mctsState GameSearch.empty
    let (Drop m) = bestAction finalNode mctsState
    lift $ catchError (Just <$> makeMove playerId m) handleIllegalMoveError >> return m


getAndPostUserMove :: ClientStateM Int
getAndPostUserMove = do
    liftIO $ print "Provide a move (0-6):"
    userMove <- liftIO (readMaybe <$> getLine :: IO (Maybe Int))
    case userMove of
        Nothing -> liftIO (print "Input is not a number") >> getAndPostUserMove
        Just m | m > 6 || m < 0 -> liftIO (print "Move not in correct range") >> getAndPostUserMove
        Just m -> do
            (State playerId _ _) <- get
            resp <- lift $ catchError (Just <$> makeMove playerId m) handleIllegalMoveError
            case resp of
                Nothing -> getAndPostUserMove
                _       -> return m

handleIllegalMoveError :: ClientError -> ClientM (Maybe ())
handleIllegalMoveError (FailureResponse _ (Response code _ _ body))
    | code == status409 = liftIO (print body) >> return Nothing
handleIllegalMoveError err = throwError err

getPostUpdateUserMove getAndPostMove = do
    move <- getAndPostMove
    modify $ updateBoardAndGamePart move Yellow

    return (Just QueryingOpponentMove)


queryForOpponentMove = do
    liftIO $ print "Query opponent move"
    (m, t) <- retrieveOpponentMove
    modify $ updateBoardAndGamePart m Red
    case t of
        YourTurn -> return (Just MakingMove)
        _        -> liftIO (print "Game outcome:") >> return (Just GameOutcome)

updateBoardAndGamePart m c (State pId mctsState node) = State pId (apply (Drop m) mctsState) (fromJust $ child (Drop m) node)

retrieveOpponentMove = do
    liftIO $ threadDelay 2000
    (State playerId mctsState node) <- get
    (OpponentMove m t) <- lift . getOpponentMove $ playerId
    case (m, t) of
        (Just m', YourTurn) -> return (m', t)
        (Just m', GameOver) -> return (m', GameOver)
        _                   -> retrieveOpponentMove

registerPlayerForGame = do
    gr@(GameRegistration userId t) <- lift registerGame
    liftIO $ print userId
    modify $ fillPlayerIdAndMoveToPart userId
    case t of
        YourTurn -> return $ Just MakingMove
        WaitForTurn -> return $ Just QueryingOpponentMove
  where
    fillPlayerIdAndMoveToPart :: Text -> ClientState -> ClientState
    fillPlayerIdAndMoveToPart userId (State _ mctsState node) = State (Just userId) mctsState node
