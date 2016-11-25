-------------------------------------------------------------------------------
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

import System.Random (RandomGen, randomR)

--------------------------------------------------------------------------------
-- ## Local

import Utils.Types.Route (Routes)
import qualified Utils.Types.Route as Route (AS)
import qualified Utils.Censor as Censor
import qualified Utils.AS as AS
import qualified Data.HashMap.Strict as HM (keys, lookup, size, foldrWithKey)


--------------------------------------------------------------------------------
-- # AS Subgame: Best Response Dynamics
--------------------------------------------------------------------------------

bestResponseDynamics :: RandomGen g => Routes -> AS.Players -> g -> AS.Players
bestResponseDynamics routes players randGen
  | isPNE routes updatedPlayers = updatedPlayers
  | otherwise = bestResponseDynamics routes updatedPlayers newRandGen
  where
    (updatedPlayers, newRandGen) = decoysDecide 1000 routes players randGen

isPNE :: Routes -> AS.Players -> Bool
isPNE routes players@(AS.Players _ coalition) =
    HM.foldrWithKey
      (\as action bool -> isPlayerPNE routes players as action && bool) True
      coalition

isPlayerPNE :: Routes -> AS.Players -> Route.AS -> AS.Action -> Bool
isPlayerPNE routes players as action =
    currentUtility >= alternateUtility
  where
    currentUtility   = AS.utility players routes player
    flippedPlayer    = AS.flipDecoyStatus player
    alternateUtility = AS.utility players routes flippedPlayer
    player           = AS.Player as action

decoysDecide :: RandomGen g => Int -> Routes -> AS.Players -> g -> (AS.Players, g)
decoysDecide n routes players randGen
  | n > 0     = decoysDecide (n - 1) routes updatedPlayers newRandGen
  | otherwise = (updatedPlayers, randGen)
  where
    (chosenPlayer, newRandGen) = pickRandomPlayer players randGen
    updatedPlayers = decoyDecide routes players chosenPlayer

decoyDecide :: Routes -> AS.Players -> AS.Player -> AS.Players
decoyDecide routes players player
  | currentUtility >= alternateUtility = players
  | otherwise = AS.updatePlayer players flippedPlayer
  where
    currentUtility   = AS.utility players routes player
    flippedPlayer    = AS.flipDecoyStatus player
    alternateUtility = AS.utility players routes flippedPlayer

pickRandomPlayer :: RandomGen g => AS.Players -> g -> (AS.Player, g)
pickRandomPlayer players randGen =
  case (HM.lookup randomAS coalition) of
    Just action -> (AS.Player randomAS action, newRandGen)
    Nothing     -> (AS.Player randomAS AS.NotDecoy, newRandGen) -- should not happen
  where
    (randomIndex, newRandGen) =
      randomR (0, HM.size coalition - 1) randGen
    randomAS = HM.keys coalition !! randomIndex
    coalition = AS.coalition players

