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

utility :: Player -> AS.Coalition -> Utility
utility (Player profile routes) asCoalition =
    (b0 profile) * benefit routes asCoalition
  - cost profile routes

-- ### Benefit

benefit :: Fractional a => Routes -> AS.Coalition -> a
benefit = censorshipMetric

-- data Game = Game CensorPlayer (Set AS) Routes Costs
censorshipMetric :: Fractional a => Routes -> AS.Coalition -> a
censorshipMetric routes coalition = 1 - freedomMetric routes coalition

freedomMetric :: Fractional a => Routes -> AS.Coalition -> a
freedomMetric routes coalition =
    (foldr ((+) . routeContribution coalition) 0 routes) `floatDiv` totalSize
  where
    totalSize = foldr ((+) . routeSize) 0 routes

routeContribution :: AS.Coalition -> Route -> Int
routeContribution coalition route@(Route _ source _ sink)
  | hasDecoy coalition route = routeSize route
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

