--------------------------------------------------------------------------------
-- | 
-- Module      : GameFormulation
-- Note        : 
-- 
-- Game implementation
-- 
--------------------------------------------------------------------------------

module Game where

--------------------------------------------------------------------------------
-- # Imports

--------------------------------------------------------------------------------
-- ## Local

import qualified Utils.Censor as Censor
import qualified Utils.AS as AS
import Utils.Types.Route (Routes)


--------------------------------------------------------------------------------
-- # AS Subgame: Best Response Dynamics
--------------------------------------------------------------------------------

bestResponseDynamics :: Routes -> AS.Players -> AS.Players
bestResponseDynamcis routes players =

