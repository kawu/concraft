{-# LANGUAGE RecordWildCards #-}

-- | Types and functions related to the morphosyntax data layer.

module NLP.Concraft.Morphosyntax
( 
-- * Morphosyntax data
  Sent
, Word (..)
, mapWord
, mapSent
, interpsSet
, interps
-- * Weighted collection
, WMap (unWMap)
, mkWMap
, mapWMap
) where

import Control.Arrow (first)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T


-- | A sentence of 'Word's.
type Sent t = [Word t]

-- | A word parametrized over a tag type.
data Word t = Word {
    -- | Orthographic form.
      orth      :: T.Text
    -- | Set of word interpretations.  To each interpretation
    -- a "weight of correctness within the context" is assigned.
    , tagWMap   :: WMap t
    -- | Out-of-vocabulary (OOV) word, i.e. word unknown to the
    -- morphosyntactic analyser.
    , oov       :: Bool }
    deriving (Show, Eq, Ord)

-- | Map function over word tags.
mapWord :: Ord b => (a -> b) -> Word a -> Word b
mapWord f w = w { tagWMap = mapWMap f (tagWMap w) }

-- | Map function over sentence tags.
mapSent :: Ord b => (a -> b) -> Sent a -> Sent b
mapSent = map . mapWord

-- | Interpretations of the word.
interpsSet :: Word t -> S.Set t
interpsSet = M.keysSet . unWMap . tagWMap

-- | Interpretations of the word.
interps :: Word t -> [t]
interps = S.toList . interpsSet


-- | A weighted collection of type @a@ elements.
newtype WMap a = WMap { unWMap :: M.Map a Double }
    deriving (Show, Eq, Ord)

-- | Make a weighted collection.
mkWMap :: Ord a => [(a, Double)] -> WMap a
mkWMap = WMap . M.fromListWith (+) . filter ((>=0).snd)

-- | Map function over weighted collection elements. 
mapWMap :: Ord b => (a -> b) -> WMap a -> WMap b
mapWMap f = mkWMap . map (first f) . M.toList . unWMap
