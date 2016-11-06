--------------------------------------------------------------------------------
-- | 
-- Module      : GameFormulation
-- Note        : 
-- 
-- Utility functions/etc. that describe the game.
-- 
--------------------------------------------------------------------------------

module Game where


--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import Data.Function (on)
import Data.List (foldr)
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM

--------------------------------------------------------------------------------
-- ## Local

import Types


--------------------------------------------------------------------------------
-- # Censor
--------------------------------------------------------------------------------

-- utilityCensor :: CensorPlayer -> Utility
-- utilityCensor (CensorPlayer profile routes costs) = benefitCensor routes

benefitCensor :: ASPlayers -> Routes -> Float
benefitCensor = censorshipMetric

-- data Game = Game CensorPlayer (Set AS) Routes Costs
censorshipMetric :: ASPlayers -> Routes -> Float
censorshipMetric players routes = 1 - freedomMetric players routes

freedomMetric :: ASPlayers -> Routes -> Float
freedomMetric players routes =
    (foldr ((+) . routeContribution players) 0 routes) `floatDiv` totalSize
  where
    totalSize = foldr ((+) . routeSize) 0 routes

routeContribution :: ASPlayers -> Route -> Int
routeContribution players route@(Route _ source _ sink)
  | hasDecoy players route = routeSize route
  | otherwise = 0


--------------------------------------------------------------------------------
-- # Route Operations
--------------------------------------------------------------------------------

-- Route size (not number of ASes, but size of endpoints multiplied)
routeSize :: Route -> Int
routeSize (Route _ source _ sink) = asSize source * asSize sink

-- check whether route has a Decoy
hasDecoy :: ASPlayers -> Route -> Bool
hasDecoy players (Route _ _ seq sink) =
    foldr ((||) . isDecoy players) False ases
  where
    end = Seq.length seq
    ases = Seq.insertAt end sink seq

--  check whether (Free)AS is a Decoy router
isDecoy :: ASPlayers -> AS -> Bool
isDecoy _ (CensorAS _) = False
isDecoy players (FreeAS domain) =
  case (domain `HM.lookup` players) of
    Just Decoy -> True
    _          -> False


--------------------------------------------------------------------------------
-- # Etc
--------------------------------------------------------------------------------

-- divide integrals (e.g. ints), get a fractional (e.g. float)
floatDiv :: (Integral a, Fractional c) => a -> a -> c
floatDiv = (/) `on` fromIntegral

