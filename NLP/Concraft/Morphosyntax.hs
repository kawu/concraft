module NLP.Concraft.Morphosyntax
( Word (..)
, Sent
, Choice
, Positive (unPositive)
, (<+>)
, mkPositive
, best
, known
) where

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
