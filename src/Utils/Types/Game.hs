--------------------------------------------------------------------------------
-- | 
-- Module      : Types.Game
-- Note        : Types for the overall Game formulation and Players
-- 
-- 
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- # Exports
--------------------------------------------------------------------------------

module Utils.Types.Game ( Game (..)
                        , Utility
                        ) where


--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import qualified Utils.Types.Players.Censor as Censor (Player)
import qualified Utils.Types.Players.AS as AS (Players)

--------------------------------------------------------------------------------
-- # General
--------------------------------------------------------------------------------

data Game = Game Censor.Player AS.Players

type Utility = Float

