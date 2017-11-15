{-# LANGUAGE TupleSections #-}


-- | Alignment and synchronization.  Currently works only with positional tagsets.


module NLP.Concraft.Morphosyntax.Align
( align
, sync
) where


import           Prelude hiding (Word)
import           Control.Applicative ((<|>))
import           Data.Maybe (fromJust)
import           Data.List (find)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Tagset.Positional as P

import           NLP.Concraft.Morphosyntax


-- | Synchronize two datasets, taking disamb tags from the first one
-- and the rest of information form the second one.
-- In case of differences in token-level segmentation, reference segmentation
-- (token-level) is assumed.  Otherwise, it would be difficult to choose
-- correct disamb tags.
sync :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> [Seg w P.Tag]
sync tagset xs ys = concatMap (uncurry (moveDisamb tagset)) (align xs ys)


-- | If both arguments contain only one segment, insert disamb interpretations
-- from the first segment into the second segment.  Otherwise, the first list
-- of segments will be returned unchanged.
moveDisamb :: P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> [Seg w P.Tag]
moveDisamb tagset [v] [w] =
    [w {tags = mkWMap (map (,0) tagsNew ++ disambNew)}]
  where
    -- Return list of (tag, weight) pairs assigned to the segment.
    tagPairs    = M.toList . unWMap . tags
    -- New tags domain.
    tagsNew     = map fst (tagPairs w)
    -- Disamb list with tags mapped to the new domain.
    disambNew   = [(newDom x, c) | (x, c) <- tagPairs v, c > 0]
    -- Find corresonding tag in the new tags domain.
    newDom tag  = fromJust $
            find ( ==tag) tagsNew   -- Exact match
        <|> find (~==tag) tagsNew   -- Expanded tag match
        <|> Just tag                -- Controversial
      where
        x ~== y = S.size (label x `S.intersection` label y) > 0
        label   = S.fromList . P.expand tagset
-- Do nothing in this case.
moveDisamb _ xs _ = xs


-- | Align two lists of segments.
align :: Word w => [Seg w t] -> [Seg w t] -> [([Seg w t], [Seg w t])]
align [] [] = []
align [] _  = error "align: null xs, not null ys"
align _  [] = error "align: not null xs, null ys"
align xs ys =
    let (x, y) = match xs ys
        rest   = align (drop (length x) xs) (drop (length y) ys)
    in  (x, y) : rest


-- | Find the shortest, length-matching prefixes in the two input lists.
match :: Word w => [Seg w t] -> [Seg w t] -> ([Seg w t], [Seg w t])
match xs' ys' =
    doIt 0 xs' 0 ys'
  where
    doIt i (x:xs) j (y:ys)
        | n == m    = ([x], [y])
        | n <  m    = addL x $ doIt n xs j (y:ys)
        | otherwise = addR y $ doIt i (x:xs) m ys
      where
        n = i + size x
        m = j + size y
    doIt _ [] _ _   = error "match: the first argument is null"
    doIt _ _  _ []  = error "match: the second argument is null"
    size w = T.length . T.filter (not.C.isSpace) $ orth w
    addL x (xs, ys) = (x:xs, ys)
    addR y (xs, ys) = (xs, y:ys)
