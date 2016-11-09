--------------------------------------------------------------------------------
-- | 
-- Module      : Utils.Types.Players.AS
-- Note        : 
-- 
-- Definitions for the ASPlayers
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- # Exports
--------------------------------------------------------------------------------

module Utils.Types.Players.AS ( Player (..)
                              , Players (..)
                              , Params (..)
                              , ServiceFee
                              , DollarsPerUnit
                              , Actions
                              , Action (..)
                              ) where


--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import qualified Data.HashMap.Strict as HM

--------------------------------------------------------------------------------
-- ## Local

import Utils.Types.Route (AS)

--------------------------------------------------------------------------------
-- # AS Players
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Types

data Player = Player { domain :: AS
                     , action :: Action
                     } deriving (Show)

data Players = Players { params  :: Params
                       , actions :: Actions
                       } deriving (Show)

type Actions = HM.HashMap AS Action

data Params = Params { dollarsPerUnit :: DollarsPerUnit
                     , serviceFee     :: ServiceFee
                     } deriving (Show)

type DollarsPerUnit = Float
type ServiceFee = Float

-- Free AS's choice to deploy as decoy router or not
data Action = Decoy | NotDecoy
  deriving (Eq, Enum, Show)

