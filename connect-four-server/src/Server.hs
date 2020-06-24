{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Server
Description : Connect Four game server
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

Module implemented for Functional Programming in Haskell 2020 course at University of Warsaw.
-}
module Server (
  initializeState
  ,app
)where

import           Control.Monad.Trans.Reader  (runReaderT)
import           Data.Text                   (Text)
import           Servant
import           Servant.Swagger             
import           Data.Swagger                (Swagger)
import           Data.Swagger.Declare
import           Data.Swagger.Lens
import           Data.Swagger.Operation
import           Control.Lens.Operators      ((.~), (?~), (&))
import           DataContract
import           Handlers
import           GHC.Conc
import           System.Random
import           GameBoard

-- | Type representing connect four server API
type ConnectFourAPI = "game" :> "register" :> Post '[JSON] GameRegistration
  :<|> "game" :> "move" :> Header "userId" Text :> Capture "m" Int :> Post '[JSON] ()
  :<|> "game" :> "move" :> Header "userId" Text :> Get '[JSON] OpponentMove
  :<|> "game" :> "result" :> Header "userId" Text :> Get '[JSON] GameResult

-- | Type representing endpoint for downloading swagger json
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Type representing complete server API
type API =
  SwaggerAPI :<|>
  ConnectFourAPI



server :: ServerT API AppM
server =
  swaggerHandler 
  :<|> regHandler
  :<|> makeMoveHandler 
  :<|> getOpponentMove
  :<|> getGameResult

swaggerHandler :: AppM Swagger
swaggerHandler = return $ toSwagger (Proxy::Proxy ConnectFourAPI)
  & info. title .~ "Connect four API"
  & info. version .~ "0.1"
  & info. description ?~ "API for Connect Four game server"

connectFourAPI :: Proxy API
connectFourAPI = Proxy

nt :: ServerState -> AppM a -> Handler a
nt s x = runReaderT x s

-- | Function returning starting server state shared across all handlers
initializeState = do
  stdGen <- getStdGen
  atomically $ do
    rndGen' <- newTVar stdGen
    gs' <- newTVar (Game createEmptyBoard Ongoing)
    p1' <- newTVar Nothing
    p2' <- newTVar Nothing
    lm' <- newTVar Nothing
    np' <- newTVar Red
    return $ SState gs' p1' p2' lm' np' rndGen'

-- | Function for hosting api initialized with server state
app :: ServerState -- ^ Starting server state
    -> Application -- ^ Servant type representing server
app s = serve connectFourAPI $ hoistServer connectFourAPI (nt s) server