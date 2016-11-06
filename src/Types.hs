--------------------------------------------------------------------------------
-- | 
-- Module      : Types
-- Note        : Types for the overall Game formulation and Players
-- 
-- 
-- 
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- # Extensions
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------

module Types ( Game (..)
             , Set
             , Utility
             , Route (..)
             , Routes
             , RouteType (..)
             , AS (..)
             , makeDomain
             , asSize
             , CensorPlayer (..)
             , ASPlayers
             , ASAction (..)
    ) where


--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import Data.Data (Data)
import Data.Bits (xor)
import Data.List (foldl')
import Data.Sequence (Seq)
import Data.Hashable (Hashable, hash, hashWithSalt, hashUsing)
import Data.IP (IPv4, fromIPv4)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM


--------------------------------------------------------------------------------
-- # General
--------------------------------------------------------------------------------

data Game = Game CensorPlayer ASPlayers
type Set = HS.HashSet

type Utility = Float


--------------------------------------------------------------------------------
-- # Route/AS abstractions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Route

-- a Route is a path from ASi to ASj, via a Sequence of ASes
-- specifically, a Route goes from an ASCensor to an ASFree, through a Sequence
-- of either type of AS. it is either a BGP or RBGP route, as chosen by the
-- CensorPlayer
data Route = Route RouteType AS (Seq AS) AS
  deriving (Data, Eq, Show)

type Routes = Set Route

data RouteType = BGP | RBGP
  deriving (Data, Enum, Eq, Show)

--------------------------------------------------------------------------------
-- ### Instance declarations

instance Hashable Route where
  hashWithSalt s (Route routeType source seq sink) =
    s `combine` (hash routeType) `combine` (hash source) `combine` (hash seq)
    `combine` (hash sink)

instance Hashable RouteType where
  hashWithSalt = hashUsing fromEnum

--------------------------------------------------------------------------------
-- ## AS

data AS = FreeAS ASDomain
        | CensorAS ASDomain
  deriving (Data, Eq, Show)

type ASDomain = (IPv4, NetMask)
type NetMask = Int

makeDomain :: String -> NetMask -> ASDomain
makeDomain ip mask = ((read ip :: IPv4), mask)

-- accessor
domain :: AS -> ASDomain
domain (FreeAS domain) = domain
domain (CensorAS domain) = domain

-- get number of IP addrs from integer netmask
asSize :: AS -> ASSize
asSize (FreeAS   (_, mask)) = 2 ^ (32 - mask)
asSize (CensorAS (_, mask)) = 2 ^ (32 - mask)

--------------------------------------------------------------------------------
-- # Players
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Censor

data CensorPlayer = CensorPlayer CensorProfile Routes CensorCosts

-- Censor's profile is made up of 6 values, each between 0 and 1
data CensorProfile = CensorProfile Float Float Float Float Float Float
data CensorCosts = CensorCosts Cost Cost Cost Cost Cost Cost
data Cost = Float

--------------------------------------------------------------------------------
-- ## AS Players

-- ### Types

type ASPlayers = HM.HashMap ASDomain ASAction

-- Free AS's choice to deploy as decoy router or not
data ASAction = Decoy | NotDecoy
  deriving (Eq, Data, Enum, Show)

type ASSize = Int
type Decoy = Bool

--------------------------------------------------------------------------------
-- ### Instance declarations

instance Hashable AS where
  hashWithSalt s (FreeAS (ip, range)) =
    s `combine` (hash ip) `combine` (hash range)
  hashWithSalt s (CensorAS (ip, range)) =
    s `combine` (hash ip) `combine` (hash range)

instance Hashable IPv4 where
  hash ip = hash (fromIPv4 ip)

instance Hashable ASAction where
  hashWithSalt = hashUsing fromEnum

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2


--------------------------------------------------------------------------------
-- # Etc
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Instance declarations

data SPInt = SP !Int !Int

-- hashing a Foldable. overlaps with instance Hashable a => Hashable [a]
instance {-# OVERLAPS #-} (Foldable f, Hashable h) => Hashable (f h) where
  hashWithSalt salt f =
      finalise (foldl' step (SP salt 0) f)
    where
      finalise (SP s l) = hashWithSalt s l
      step (SP s l) x   = SP (hashWithSalt s x) (l + 1)

