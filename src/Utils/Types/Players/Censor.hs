--------------------------------------------------------------------------------
-- | 
-- Module      : Utils.Types.Players.Censor
-- Note        : 
-- 
-- Censor Player definition
-- 
--------------------------------------------------------------------------------

module Utils.Types.Players.Censor ( Player (..)
                                  , Profile (..)
                                  ) where

--------------------------------------------------------------------------------
-- # Imports

--------------------------------------------------------------------------------
-- ## Local

import Utils.Types.Route (Routes)


--------------------------------------------------------------------------------
-- # Censor

data Player = Player { profile :: Profile
                     , routes  :: Routes 
                     } deriving (Show)

-- data Params = Params { profile :: Profile }
--   deriving (Show)

-- Censor's profile is made up of 6 values, each between 0 and 1
data Profile = Profile { b0 :: BenefitWeight
                       , b1 :: CostWeight
                       , b2 :: CostWeight
                       , b3 :: CostWeight
                       , b4 :: CostWeight
                       , b5 :: CostWeight
                       , b6 :: CostWeight
                       } deriving (Show)

type BenefitWeight = Float
type CostWeight = Float

