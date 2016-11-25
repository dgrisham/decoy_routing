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
                              , Coalition
                              , Action (..)
                              ) where


--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import qualified Data.HashMap.Strict as HM

--------------------------------------------------------------------------------
-- ## Local

import qualified Utils.Types.Route as Route (AS)


--------------------------------------------------------------------------------
-- # AS Players
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Types

data Player = Player { domain :: Route.AS
                     , action :: Action
                     } deriving (Show)

data Players = Players { params    :: Params
                       , coalition :: Coalition
                       } deriving (Show)

type Coalition = HM.HashMap Route.AS Action

data Params = Params { dollarsPerUnit :: DollarsPerUnit
                     , serviceFee     :: ServiceFee
                     } deriving (Show)

type DollarsPerUnit = Float
type ServiceFee = Float

-- Free AS's choice to deploy as decoy router or not
data Action = Decoy | NotDecoy
  deriving (Eq, Enum, Show)

