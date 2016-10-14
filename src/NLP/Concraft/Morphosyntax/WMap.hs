{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module NLP.Concraft.Morphosyntax.WMap
( WMap (unWMap)
, mapWMap
, mkWMap
) where


import           Data.Binary (Binary)
import qualified Data.Map as M
import           Control.Arrow (first)


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
