{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Morphosyntax
( Word (..)
, mapWord
, Sent
, Choice
, mapChoice
, Positive (unPositive)
, (<+>)
, mkPositive
, best
, known
) where

import Control.Arrow (first)
import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

-- | A word parametrized over the tag type.
data Word t = Word {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Set of word interpretations.
    , tags  :: S.Set t }
    deriving (Show, Read, Eq, Ord)

mapWord :: Ord b => (a -> b) -> Word a -> Word b
mapWord f Word{..} = Word
    { orth = orth
    , tags = S.fromList . map f . S.toList $ tags }

-- | A sentence of 'Word's.
type Sent t = [Word t]

-- | Interpretations chosen in the given context with
-- corresponding positive weights.
type Choice t = M.Map t (Positive Double)

-- | Positive number.
newtype Positive a = Positive { unPositive :: a }
    deriving (Show, Eq, Ord)

(<+>) :: Num a => Positive a -> Positive a -> Positive a
Positive x <+> Positive y = Positive (x + y)
{-# INLINE (<+>) #-}

mapChoice :: Ord b => (a -> b) -> Choice a -> Choice b
mapChoice f = M.fromListWith (<+>) . map (first f) . M.toList

mkPositive :: (Num a, Ord a) => a -> Positive a
mkPositive x
    | x > 0     = Positive x
    | otherwise = error "mkPositive: not a positive number"
{-# INLINE mkPositive #-}

-- | Retrieve the most probable interpretation.
best :: Choice t -> t
best c
    | M.null c  = error "best: null choice" 
    | otherwise = fst . maximumBy (comparing snd) $ M.toList c

-- | A word is considered to be known when the set of possible
-- interpretations is not empty.
known :: Word t -> Bool
known = not . S.null . tags
{-# INLINE known #-}
