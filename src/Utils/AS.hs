--------------------------------------------------------------------------------
-- | 
-- Module      : Utils.AS
-- Note        : 
-- 
-- Useful functions for evaluating Routes
-- 
--------------------------------------------------------------------------------

module Utils.AS where

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

import Utils.Types.Game (Utility)
import qualified Utils.Types.Players.AS as ASP
import Utils.Types.Route (Routes, Route (..), routeSize, AS (..))


--------------------------------------------------------------------------------
-- # AS Utility Function
--------------------------------------------------------------------------------

utilityASPlayer :: Fractional a => ASP.Players -> ASP.Player -> Routes -> Utility
utilityASPlayer players player routes =
    foldr ((+) . routeRevenue players player) 0 routes

routeRevenue :: Fractional a => ASP.Players -> ASP.Player -> Route -> Utility
routeRevenue (ASP.Players params actions) player route@(Route routeType _ _ _)
  | isOnRoute player route = routeSize' * (transitRev + decoyRev)
  | otherwise = 0
  where
    dollarConv = ASP.dollarsPerUnit params
    routeSize' = fromIntegral $ routeSize route
    transitRev = transitRevenue player route
    serviceFee = ASP.serviceFee params
    decoyRev   = decoyRevenue actions serviceFee player route

transitRevenue :: Fractional a => ASP.Player -> Route -> Utility
transitRevenue (ASP.Player _ _) route = 1

decoyRevenue :: Fractional a => ASP.Actions -> ASP.ServiceFee -> ASP.Player -> Route -> ASP.ServiceFee
decoyRevenue actions serviceFee player@(ASP.Player as action) route
  | isFirstDecoy actions player route = serviceFee
  | otherwise = 0

isFirstDecoy :: ASP.Actions -> ASP.Player -> Route -> Bool
isFirstDecoy actions (ASP.Player as _) route
  | isNothing firstDecoy = False
  | isDecoy actions as = (as == fromJust firstDecoy)
  | otherwise = False
  where
    firstDecoy = firstDecoyOnRoute actions route

firstDecoyOnRoute :: ASP.Actions -> Route -> Maybe AS
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
isOnRoute :: ASP.Player -> Route -> Bool
isOnRoute (ASP.Player as _) (Route _ _ seq sink) =
  case (Seq.elemIndexL as ases) of
    Just x -> True
    Nothing -> False
  where
    ases = seq |> sink

-- check whether route has a Decoy
hasDecoy :: ASP.Actions -> Route -> Bool
hasDecoy actions (Route _ _ seq sink) =
    foldr ((||) . isDecoy actions) False ases
  where
    ases = seq |> sink

--  check whether (Free)AS is a Decoy router
isDecoy :: ASP.Actions -> AS -> Bool
isDecoy _ (CensorAS _) = False
isDecoy actions as =
  case (HM.lookup as actions) of
    Just ASP.Decoy -> True
    _             -> False

