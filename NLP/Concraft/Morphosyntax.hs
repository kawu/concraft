{-# LANGUAGE RecordWildCards #-}

-- | Types and functions related to the morphosyntax data layer.

module NLP.Concraft.Morphosyntax
( 
-- * Morphosyntax
  Sent
, Word (..)
, mapWord
-- * Probability
, Prob (unProb)
, mkProb
, mapProb
) where


import Control.Arrow (first)
import qualified Data.Map as M
import qualified Data.Text as T


-- | A sentence of 'Word's.
type Sent t = [Word t]

-- | A word parametrized over a tag type.
data Word t = Word {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Set of word interpretations.
    , tags  :: Prob t
    -- | Out-of-vocabulary (OOV) word, i.e. word unknown to the
    -- morphosyntactic analyser.
    , oov   :: Bool }
    deriving (Show, Eq, Ord)

-- | Map function over word tags.
mapWord :: Ord b => (a -> b) -> Word a -> Word b
mapWord f w = w { tags = mapProb f (tags w) }


-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equal to 0.
newtype Prob a = Prob { unProb :: M.Map a Double }
    deriving (Show, Eq, Ord)

-- | Make a probability distribution.
mkProb :: Ord a => [(a, Double)] -> Prob a
mkProb =
    Prob . normalize . M.fromListWith (+)
  where
    normalize dist =
        let z = sum (M.elems dist)
        in  fmap (/z) dist

-- | Map function over probability elements. 
mapProb :: Ord b => (a -> b) -> Prob a -> Prob b
mapProb f = mkProb . map (first f) . M.toList . unProb

-- -- | Retrieve the most probable interpretation.
-- best :: Choice t -> t
-- best c
--     | M.null c  = error "best: null choice" 
--     | otherwise = fst . maximumBy (comparing snd) $ M.toList c
