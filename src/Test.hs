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

import Game
import Utils.Types.Players.Censor
import Utils.Types.Route (Route (..), RouteType (..), AS (..), makeDomain)
import qualified Utils.Types.Players.AS as AS


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

routeWithDecoy = Route BGP censorAS (Seq.fromList [freeAS1]) freeAS3
routeWithoutDecoy = Route BGP censorAS (Seq.fromList [freeAS2]) freeAS3

asPlayers :: AS.Players
asPlayers = AS.Players { AS.params = asParams, AS.coalition = asActions }

asParams :: AS.Params
asParams = AS.Params { AS.dollarsPerUnit = 1, AS.serviceFee = 0.5 }

asActions :: AS.Coalition
asActions = (HM.insert freeAS1 AS.Decoy) $ (HM.insert freeAS2 AS.NotDecoy HM.empty)

