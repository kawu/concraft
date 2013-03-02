{-# LANGUAGE RecordWildCards #-}

-- | Types and functions related to the morphosyntax data layer.

module NLP.Concraft.Morphosyntax
( 
-- * Basic types
  Sent
, mapSent
, Seg (..)
, mapSeg
, interpsSet
, interps
-- * Weighted collection
, WMap (unWMap)
, mapWMap
, mkWMap
-- * Alignment and synchronization
, match
) where

import Control.Arrow (first)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T

--------------------------
-- Morphosyntax data layer
--------------------------

-- | A sentence. 
type Sent t = [Seg t]

-- | Map function over sentence tags.
mapSent :: Ord b => (a -> b) -> Sent a -> Sent b
mapSent = map.mapSeg

-- | A segment parametrized over a tag type.
data Seg t = Seg {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Set of segment interpretations.  To each interpretation
    -- a weight of appropriateness within the context
    -- is assigned.
    , tags  :: WMap t
    -- | Out-of-vocabulary (OOV) segment, i.e. segment unknown to the
    -- morphosyntactic analyser.
    , oov   :: Bool }
    deriving (Show, Eq, Ord)

-- | Map function over segment tags.
mapSeg :: Ord b => (a -> b) -> Seg a -> Seg b
mapSeg f w = w { tags = mapWMap f (tags w) }

-- | Interpretations of the segment.
interpsSet :: Seg t -> S.Set t
interpsSet = M.keysSet . unWMap . tags

-- | Interpretations of the segment.
interps :: Seg t -> [t]
interps = S.toList . interpsSet

----------------------
-- Weighted collection
----------------------

-- | A set with a non-negative weight assigned to each of
-- its elements.
newtype WMap a = WMap { unWMap :: M.Map a Double }
    deriving (Show, Eq, Ord)

-- | Make a weighted collection.  Negative elements will be ignored.
mkWMap :: Ord a => [(a, Double)] -> WMap a
mkWMap = WMap . M.fromListWith (+) . filter ((>=0).snd)

-- | Map function over weighted collection elements. 
mapWMap :: Ord b => (a -> b) -> WMap a -> WMap b
mapWMap f = mkWMap . map (first f) . M.toList . unWMap

--------------------------------
-- Alignment and synchronization
--------------------------------

-- -- | Synchronize two datasets, taking disamb tags from the first one
-- -- and the rest of information form the second one.
-- sync :: P.Tagset -> [Seg] -> [Seg] -> [Seg]
-- sync tagset xs ys = concatMap (uncurry (syncWord tagset)) (align xs ys)
-- 
-- syncWord :: Tagset -> [Seg] -> [Seg] -> [Seg]
-- syncWord tagset [v] [w] =
--     [Disamb (word w) mlt]
--   where
--     mlt = mergeMulti (interps $ word w) (choice v)
--     mergeMulti xs = concatMap (mergeDisamb xs)
--     mergeDisamb xs (x, pr)
--         | Just x' <- find ( ==x) xs = [(x', pr)]    -- ^ Exact match
--         | Just x' <- find (~==x) xs = [(x', pr)]    -- ^ Expanded tag match
--         | otherwise                 = [(x , pr)]    -- ^ Controversial
--       where
--         x ~== y = S.size (label x `S.intersection` label y) > 0
--         label   = S.fromList . expand tagset . tag
-- syncWord tagset xs ys = xs
-- 
-- align :: [Seg] -> [Seg] -> [([Seg], [Seg])]
-- align [] [] = []
-- align [] ys = error "align: null xs, not null ys"
-- align xs [] = error "align: not null xs, null ys"
-- align xs ys =
--     let (x, y) = match xs ys
--     in  (x, y) : align (drop (length x) xs) (drop (length y) ys)

-- | Find the shortest, length-matching prefixes of the two input lists.
match :: [Seg t] -> [Seg t] -> ([Seg t], [Seg t])
match xs ys =
    doIt 0 xs 0 ys
  where
    doIt i (x:xs) j (y:ys)
        | n == m = ([x], [y])
        | n <  m = x <: doIt n xs j (y:ys)
        | n >  m = y >: doIt i (x:xs) m ys
      where
        n = i + size x
        m = j + size y
    size w = T.length . T.filter (not.C.isSpace) $ orth w
    x <: (xs, ys) = (x:xs, ys)
    y >: (xs, ys) = (xs, y:ys)
