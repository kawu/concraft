module NLP.Concraft.Morphosyntax
( Word (..)
, Sent
, Interp (..)
, Choice
, Positive (unPositive)
, mkPositive
, known
, best
) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

-- | A word parametrized over the tag type.
data Word t = Word {
    -- | Orthographic form.
      orth      :: T.Text
    -- | Set of word interpretations.
    , interps   :: S.Set (Interp t) }
    deriving (Show, Read, Eq, Ord)

-- | A sentence of 'Word's.
type Sent t = [Word t]

-- | A potential interpretation of the word.
data Interp t = Interp {
    -- | Base form related to the interpretation. 
      base      :: T.Text
    -- | Morphosyntactic tag.
    , tag       :: t }
    deriving (Show, Read, Eq, Ord)

-- | Interpretations chosen in the given context with
-- corresponding positive weights.
type Choice t = M.Map (Interp t) (Positive Double)

-- | Positive number.
newtype Positive a = Positive { unPositive :: a }
    deriving (Show, Eq, Ord)

mkPositive :: (Num a, Ord a) => a -> Positive a
mkPositive x
    | x > 0     = Positive x
    | otherwise = error "mkPositive: not a positive number"
{-# INLINE mkPositive #-}

-- | Retrieve the most probable interpretation.
best :: Choice t -> Interp t
best c
    | M.null c  = error "best: null choice" 
    | otherwise = fst . maximumBy (comparing snd) $ M.toList c

-- | The word is known when the set of potential interpretations
-- is not empty.
known :: Word t -> Bool
known = not . S.null . interps
{-# INLINE known #-}
