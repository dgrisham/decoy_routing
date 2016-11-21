--------------------------------------------------------------------------------
-- | 
-- Module      : Utils.Censor
-- Note        : 
-- 
-- Useful functionality for CensorPlayer
-- 
--------------------------------------------------------------------------------

module Utils.Censor ( utility
                    , module Utils.Types.Players.Censor
                    ) where

--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import Data.Function (on)
import Data.List (foldr)

--------------------------------------------------------------------------------
-- ## Local

import Utils.AS (hasDecoy)
import Utils.Types.Game (Utility)
import Utils.Types.Route (Routes, Route (..), routeSize)
import Utils.Types.Players.Censor
import qualified Utils.Types.Players.AS as AS


--------------------------------------------------------------------------------
-- # Utility Functions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Censor

utility :: Player -> AS.Actions -> Utility
utility (Player profile routes) asActions =
    (b0 profile) * benefit routes asActions
  - cost profile routes

-- ### Benefit

benefit :: Fractional a => Routes -> AS.Actions -> a
benefit = censorshipMetric

-- data Game = Game CensorPlayer (Set AS) Routes Costs
censorshipMetric :: Fractional a => Routes -> AS.Actions -> a
censorshipMetric routes actions = 1 - freedomMetric routes actions

freedomMetric :: Fractional a => Routes -> AS.Actions -> a
freedomMetric routes actions =
    (foldr ((+) . routeContribution actions) 0 routes) `floatDiv` totalSize
  where
    totalSize = foldr ((+) . routeSize) 0 routes

routeContribution :: AS.Actions -> Route -> Int
routeContribution actions route@(Route _ source _ sink)
  | hasDecoy actions route = routeSize route
  | otherwise = 0

-- ### Cost

cost :: Profile -> Routes -> Float
cost profile routes =
    b1 profile * unreachableDestinations routes
  + b2 profile * unreachableDomains routes
  + b3 profile * increasedRouteLength routes
  + b4 profile * nonValleyFreePaths routes
  + b5 profile * lessPreferredRoutes routes
  + b6 profile * newTransitASes routes

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

