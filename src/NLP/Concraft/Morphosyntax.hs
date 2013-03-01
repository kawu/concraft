{-# LANGUAGE RecordWildCards #-}

-- | Types and functions related to the morphosyntax data layer.

module NLP.Concraft.Morphosyntax
( 
-- * Morphosyntax data
  Sent
, mapSent
, Word (..)
, mapWord
, Space (unSpace)
, space
-- , mapSent
, interpsSet
, interps
-- * Weighted collection
, WMap (unWMap)
, mapWMap
, mkWMap
) where

import Control.Arrow (first)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T

--------------------------
-- Morphosyntax data layer
--------------------------

-- | A sentence of 'Word's interleaved with space chunks.
type Sent t = [Either Space (Word t)]

-- | Map function over sentence tags.
mapSent :: Ord b => (a -> b) -> Sent a -> Sent b
mapSent = fmap.fmap.mapWord

-- | A word parametrized over a tag type.
data Word t = Word {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Set of word interpretations.  To each interpretation
    -- a weight of appropriateness within the context
    -- is assigned.
    , tags  :: WMap t
    -- | Out-of-vocabulary (OOV) word, i.e. word unknown to the
    -- morphosyntactic analyser.
    , oov   :: Bool }
    deriving (Show, Eq, Ord)

-- | Map function over word tags.
mapWord :: Ord b => (a -> b) -> Word a -> Word b
mapWord f w = w { tags = mapWMap f (tags w) }

-- | A space, ar rather a chunk of arbitrary spaces.
newtype Space = Space { unSpace :: T.Text }

-- | Smart constructor which checkes if the input argument is
-- really a space.
space :: T.Text -> Maybe Space
space x
    | T.any (not . C.isSpace) x = Nothing
    | otherwise                 = Just (Space x)

-- | Interpretations of the word.
interpsSet :: Word t -> S.Set t
interpsSet = M.keysSet . unWMap . tags

-- | Interpretations of the word.
interps :: Word t -> [t]
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
