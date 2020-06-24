{-# LANGUAGE DeriveGeneric #-}
--{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

{-|
Module      : Messages
Description : Set of types used for communication with connect four game server
Copyright   : (c) Sebastian Sowik, 2020
License     : MIT
Maintainer  : sowiks@student.mini.pw.edu.pl
Stability   : experimental
Portability : POSIX

Module implemented for Functional Programming in Haskell 2020 course at University of Warsaw.
-}
module Messages where


import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | Connect four game server
data GameRegistration = GameRegistration
  { gamerId :: Text -- ^ 
  , turnOrder :: TurnOrder -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON GameRegistration
instance ToJSON GameRegistration 

-- | 
data GameResult = YouWon | YouLost | Draw deriving (Show, Eq, Generic)

instance FromJSON GameResult
instance ToJSON GameResult

-- | 
data OpponentMove = OpponentMove
  { move :: Maybe Int -- ^ 
  , turn :: TurnOrder -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON OpponentMove
instance ToJSON OpponentMove
-- | 
data TurnOrder = YourTurn | WaitForTurn | GameOver deriving (Show, Eq, Generic)

instance FromJSON TurnOrder
instance ToJSON TurnOrder
