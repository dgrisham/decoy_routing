--------------------------------------------------------------------------------
-- | 
-- Module      : Utils.AS
-- Note        : 
-- 
-- Useful functions for evaluating Routes
-- 
--------------------------------------------------------------------------------

module Utils.AS ( utility
                , hasDecoy
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

utility :: Fractional a => Players -> Player -> Routes -> Utility
utility players player routes =
    foldr ((+) . routeRevenue players player) 0 routes

routeRevenue :: Fractional a => Players -> Player -> Route -> Utility
routeRevenue (Players params actions) player route@(Route routeType _ _ _)
  | isOnRoute player route = routeSize' * (transitRev + decoyRev)
  | otherwise = 0
  where
    dollarConv = dollarsPerUnit params
    routeSize' = fromIntegral $ routeSize route
    transitRev = transitRevenue player route
    decoyRev   = decoyRevenue actions (serviceFee params) player route

transitRevenue :: Fractional a => Player -> Route -> Utility
transitRevenue (Player _ _) route = 1

decoyRevenue :: Fractional a => Actions -> ServiceFee -> Player -> Route -> ServiceFee
decoyRevenue actions serviceFee player@(Player as action) route
  | isFirstDecoy actions player route = serviceFee
  | otherwise = 0

isFirstDecoy :: Actions -> Player -> Route -> Bool
isFirstDecoy actions (Player as _) route
  | isNothing firstDecoy = False
  | isDecoy actions as = (as == fromJust firstDecoy)
  | otherwise = False
  where
    firstDecoy = firstDecoyOnRoute actions route

firstDecoyOnRoute :: Actions -> Route -> Maybe Route.AS
firstDecoyOnRoute actions route@(Route _ _ seq sink) =
  case Seq.findIndexL (isDecoy actions) ases of
    Just k -> Seq.lookup k ases
    Nothing -> Nothing
  where
    ases = seq |> sink

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
hasDecoy :: Actions -> Route -> Bool
hasDecoy actions (Route _ _ seq sink) =
    foldr ((||) . isDecoy actions) False ases
  where
    ases = seq |> sink

--  check whether (Free)AS is a Decoy router
isDecoy :: Actions -> Route.AS -> Bool
isDecoy _ (Route.CensorAS _) = False
isDecoy actions as =
  case (HM.lookup as actions) of
    Just Decoy -> True
    _             -> False

