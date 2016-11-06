--------------------------------------------------------------------------------
-- | 
-- Module      : Test
-- 
-- Simple imports for testing in GHCi
-- 
--------------------------------------------------------------------------------

module Test where

--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM

--------------------------------------------------------------------------------
-- ## Local

import Types
import Game


--------------------------------------------------------------------------------
-- # Route Functions
--------------------------------------------------------------------------------

freeDomain1 = makeDomain "192.168.1.45" 32
freeDomain2 = makeDomain "192.168.1.48" 16
freeDomain3 = makeDomain "192.168.2.50" 32
freeAS1 = FreeAS freeDomain1
freeAS2 = FreeAS freeDomain2
freeAS3 = FreeAS freeDomain3
censorDomain = makeDomain "10.0.0.1" 16
censorAS = CensorAS censorDomain

asPlayers :: ASPlayers
asPlayers = (HM.insert freeDomain1 Decoy) $ (HM.insert freeDomain2 NotDecoy HM.empty)

routeWithDecoy = Route BGP censorAS (Seq.fromList [freeAS1]) freeAS3
routeWithoutDecoy = Route BGP censorAS (Seq.fromList [freeAS2]) freeAS3

