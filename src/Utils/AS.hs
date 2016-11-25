---------------------------------------------------------------------------------- | 
-- Module      : Utils.AS
-- Note        : 
-- 
-- Useful functions for evaluating Routes
-- 
--------------------------------------------------------------------------------

module Utils.AS ( utility
                , hasDecoy
                , updatePlayer
                , flipDecoyStatus
                , module Utils.Types.Players.AS
                ) where

--------------------------------------------------------------------------------
-- # Import
--------------------------------------------------------------------------------

import Data.List (foldr)
import Data.Sequence ((|>))
import Data.Maybe (isNothing, fromJust)
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM

--------------------------------------------------------------------------------
-- ## Local

import Utils.Types.Players.AS
import Utils.Types.Game (Utility)
import Utils.Types.Route (Routes, Route (..), routeSize)
import qualified Utils.Types.Route as Route (AS (..))


--------------------------------------------------------------------------------
-- # AS Utility Function
--------------------------------------------------------------------------------

utility :: Fractional a => Players -> Routes -> Player -> Utility
utility players routes player =
    foldr ((+) . routeRevenue players player) 0 routes

routeRevenue :: Fractional a => Players -> Player -> Route -> Utility
routeRevenue (Players params coalition) player route@(Route routeType _ _ _)
  | isOnRoute player route = routeSize' * (transitRev + decoyRev)
  | otherwise = 0
  where
    dollarConv = dollarsPerUnit params
    routeSize' = fromIntegral $ routeSize route
    transitRev = transitRevenue player route
    decoyRev   = decoyRevenue coalition (serviceFee params) player route

transitRevenue :: Fractional a => Player -> Route -> Utility
transitRevenue (Player _ _) route = 1

decoyRevenue :: Fractional a => Coalition -> ServiceFee -> Player -> Route -> ServiceFee
decoyRevenue coalition serviceFee player@(Player as action) route
  | isFirstDecoy coalition player route = serviceFee
  | otherwise = 0

isFirstDecoy :: Coalition -> Player -> Route -> Bool
isFirstDecoy coalition (Player as _) route
  | isNothing firstDecoy = False
  | isDecoy coalition as = (as == fromJust firstDecoy)
  | otherwise = False
  where
    firstDecoy = firstDecoyOnRoute coalition route

firstDecoyOnRoute :: Coalition -> Route -> Maybe Route.AS
firstDecoyOnRoute coalition route@(Route _ _ seq sink) =
  case Seq.findIndexL (isDecoy coalition) ases of
    Just k -> Seq.lookup k ases
    Nothing -> Nothing
  where
    ases = seq |> sink

updatePlayer :: Players -> Player -> Players
updatePlayer players (Player as action) =
    players { coalition = HM.insert as action (coalition players) }

--------------------------------------------------------------------------------
-- ## Helper functions
--------------------------------------------------------------------------------

-- check whether AS is on route
isOnRoute :: Player -> Route -> Bool
isOnRoute (Player as _) (Route _ _ seq sink) =
  case (Seq.elemIndexL as ases) of
    Just x -> True
    Nothing -> False
  where
    ases = seq |> sink

-- check whether route has a Decoy
hasDecoy :: Coalition -> Route -> Bool
hasDecoy coalition (Route _ _ seq sink) =
    foldr ((||) . isDecoy coalition) False ases
  where
    ases = seq |> sink

--  check whether (Free)AS is a Decoy router
isDecoy :: Coalition -> Route.AS -> Bool
isDecoy _ (Route.CensorAS _) = False
isDecoy coalition as =
  case (HM.lookup as coalition) of
    Just Decoy -> True
    _             -> False

-- change whether player is decoy or not
flipDecoyStatus :: Player -> Player
flipDecoyStatus (Player domain Decoy) = Player domain NotDecoy
flipDecoyStatus (Player domain NotDecoy) = Player domain Decoy

