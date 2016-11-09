--------------------------------------------------------------------------------
-- | 
-- Module      : Route
-- Note        : 
-- 
-- Route and AS (not the ASPlayer) definitions
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- # Extensions
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}


--------------------------------------------------------------------------------
-- # Exports
--------------------------------------------------------------------------------

module Utils.Types.Route ( Routes
                         , Route (..)
                         , Set
                         , RouteType (..)
                         , routeSize
                         , AS (..)
                         , makeDomain
                         ) where


--------------------------------------------------------------------------------
-- # Imports
--------------------------------------------------------------------------------

import Data.Data (Data)
import Data.Bits (xor)
import Data.List (foldl')
import Data.Sequence (Seq)
import Data.IP (IPv4, fromIPv4)
import Data.Hashable (Hashable, hash, hashWithSalt, hashUsing)
import qualified Data.HashSet as HS


--------------------------------------------------------------------------------
-- # Route/AS abstractions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Route

type Routes = Set Route

-- a Route is a path from ASi to ASj, via a Sequence of ASes
data Route = Route RouteType AS (Seq AS) AS
  deriving (Data, Eq, Show)

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
-- ## Useful functions

-- Route size (not number of ASes, but size of endpoints multiplied)
routeSize :: Route -> Int
routeSize (Route _ source _ sink) = asSize source * asSize sink


--------------------------------------------------------------------------------
-- # AS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ## Types

data AS = FreeAS ASDomain
        | CensorAS ASDomain
  deriving (Data, Eq, Show)

type ASDomain = (IPv4, NetMask)
type NetMask = Int

--------------------------------------------------------------------------------
-- ## Functions

makeDomain :: String -> NetMask -> ASDomain
makeDomain ip mask = ((read ip :: IPv4), mask)

-- get number of IP addrs from integer netmask
asSize :: AS -> ASSize
asSize (FreeAS   (_, mask)) = 2 ^ (32 - mask)
asSize (CensorAS (_, mask)) = 2 ^ (32 - mask)

type ASSize = Int

--------------------------------------------------------------------------------
-- ## Instance declarations

instance Hashable AS where
  hashWithSalt s (FreeAS (ip, range)) =
    s `combine` (hash ip) `combine` (hash range)
  hashWithSalt s (CensorAS (ip, range)) =
    s `combine` (hash ip) `combine` (hash range)

instance Hashable IPv4 where
  hash ip = hash (fromIPv4 ip)


--------------------------------------------------------------------------------
-- # Etc
--------------------------------------------------------------------------------

type Set = HS.HashSet

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

-- combines hashed results
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

