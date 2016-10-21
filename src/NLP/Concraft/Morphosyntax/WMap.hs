{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module NLP.Concraft.Morphosyntax.WMap
( WMap (unWMap)
, mapWMap
, mkWMap
, trim
) where


import           Control.Arrow (first)
import           Data.Binary (Binary)
import           Data.Ord (comparing)
import           Data.List (sortBy)
import qualified Data.Map as M


----------------------
-- Weighted collection
----------------------


-- | A set with a non-negative weight assigned to each of
-- its elements.
newtype WMap a = WMap { unWMap :: M.Map a Double }
    deriving (Show, Eq, Ord, Binary)


-- | Make a weighted collection.  Negative elements will be ignored.
mkWMap :: Ord a => [(a, Double)] -> WMap a
mkWMap = WMap . M.fromListWith (+) . filter ((>=0).snd)


-- | Map function over weighted collection elements.
mapWMap :: Ord b => (a -> b) -> WMap a -> WMap b
mapWMap f = mkWMap . map (first f) . M.toList . unWMap


--------------------------
-- Trimming
--------------------------


-- | Trim down the set of potential labels to `k` most probable ones.
trim :: (Ord a) => Int -> WMap a -> WMap a
trim k
  = mkWMap
  . take k
  . reverse
  . sortBy (comparing snd)
  . M.toList
  . unWMap
