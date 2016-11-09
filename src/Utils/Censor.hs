--------------------------------------------------------------------------------
-- | 
-- Module      : Utils.Censor
-- Note        : 
-- 
-- Useful functionality for CensorPlayer
-- 
--------------------------------------------------------------------------------

module Utils.Censor where

--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import Data.Function (on)
import Data.List (foldr)

--------------------------------------------------------------------------------
-- ## Local

import Utils.AS (hasDecoy)
import Utils.Types.Game (Utility)
import Utils.Types.Route (Routes, Route (..), routeSize, AS (..))
import qualified Utils.Types.Players.AS as ASP
import qualified Utils.Types.Players.Censor as Censor


--------------------------------------------------------------------------------
-- # Utility Functions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Censor

utilityCensor :: Censor.Player -> ASP.Actions -> Utility
utilityCensor (Censor.Player profile routes) asActions =
    (Censor.b0 profile) * benefitCensor routes asActions
  - costCensor profile routes

-- ### Benefit

benefitCensor :: Fractional a => Routes -> ASP.Actions -> a
benefitCensor = censorshipMetric

-- data Game = Game CensorPlayer (Set AS) Routes Costs
censorshipMetric :: Fractional a => Routes -> ASP.Actions -> a
censorshipMetric routes actions = 1 - freedomMetric routes actions

freedomMetric :: Fractional a => Routes -> ASP.Actions -> a
freedomMetric routes actions =
    (foldr ((+) . routeContribution actions) 0 routes) `floatDiv` totalSize
  where
    totalSize = foldr ((+) . routeSize) 0 routes

routeContribution :: ASP.Actions -> Route -> Int
routeContribution actions route@(Route _ source _ sink)
  | hasDecoy actions route = routeSize route
  | otherwise = 0

-- ### Cost

costCensor :: Censor.Profile -> Routes -> Float
costCensor profile routes =
    Censor.b1 profile * unreachableDestinations routes
  + Censor.b2 profile * unreachableDomains routes
  + Censor.b3 profile * increasedRouteLength routes
  + Censor.b4 profile * nonValleyFreePaths routes
  + Censor.b5 profile * lessPreferredRoutes routes
  + Censor.b6 profile * newTransitASes routes

unreachableDestinations :: Fractional a => Routes -> a
unreachableDestinations routes = 0

unreachableDomains :: Fractional a => Routes -> a
unreachableDomains routes = 0

increasedRouteLength :: Fractional a => Routes -> a
increasedRouteLength routes = 0

nonValleyFreePaths :: Fractional a => Routes -> a
nonValleyFreePaths routes = 0

lessPreferredRoutes :: Fractional a => Routes -> a
lessPreferredRoutes routes = 0

newTransitASes :: Fractional a => Routes -> a
newTransitASes routes = 0


--------------------------------------------------------------------------------
-- # Etc
--------------------------------------------------------------------------------

-- divide integrals (e.g. ints), get a fractional (e.g. float)
floatDiv :: (Integral a, Fractional c) => a -> a -> c
floatDiv = (/) `on` fromIntegral

