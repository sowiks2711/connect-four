{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : DataContract
Description : Set of types defining messages for connect four game server API
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

Module implemented for Functional Programming in Haskell 2020 course at University of Warsaw.
-}
module DataContract where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Swagger
import           GHC.Generics                (Generic)
import           Data.Typeable
import           Data.Swagger.Declare
import           Data.Swagger.Lens
import           Data.Swagger.Operation
import           Control.Lens.Operators      ((.~), (?~), (&))
import           Control.Lens.Combinators    (mapped)

data GameRegistration = Registration {
  gamerId :: String,
  turnOrder :: TurnOrder
} deriving (Eq, Show, Generic, Typeable)

instance ToJSON GameRegistration

instance ToSchema GameRegistration where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Connect four game server"

data TurnOrder = YourTurn | WaitForTurn | GameOver deriving (Eq, Show, Generic)

instance ToJSON TurnOrder
instance ToSchema TurnOrder

data OpponentMove = OpponentMove {
  move :: Maybe Int,
  turn :: TurnOrder
} deriving (Eq, Show, Generic, Typeable)

instance ToJSON OpponentMove
instance ToSchema OpponentMove

data GameResult = YouWon | YouLost | Draw deriving (Eq, Show, Generic)
instance ToJSON GameResult
instance ToSchema GameResult