{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types and functions related to the morphosyntax data layer.

module NLP.Concraft.Morphosyntax
( 
-- * Basic types
-- ** Sentence
  Sent
, SentO (..)
, mapSent
, mapSentO
-- ** Segment
, Seg (..)
, mapSeg
, interpsSet
, interps

-- * Conversion
, segTag
, segText
, sentTag
, sentText

-- * Weighted collection
, WMap (unWMap)
, mapWMap
, mkWMap

-- * Alignment and synchronization
, align
, sync
) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Arrow (first)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Tagset.Positional as P

--------------------------
-- Morphosyntax data layer
--------------------------

-- | A sentence. 
type Sent t = [Seg t]

-- | A sentence with its original, textual representation.
-- TODO: For the sake of training, SentO doesn't have to be a full-fledged
-- sentence.  In particular, it doesn't have to contain morphosyntactic
-- analysis results, just a list of words and chosen interpretations (tags).
-- However, maybe it is not necessary to change the type?
data SentO t = SentO
    { sent  :: Sent t
    , orig  :: T.Text }

-- | Map function over sentence tags.
mapSent :: Ord b => (a -> b) -> Sent a -> Sent b
mapSent = map.mapSeg

-- | Map function over sentence tags.
mapSentO :: Ord b => (a -> b) -> SentO a -> SentO b
mapSentO f x =
    let s = mapSent f (sent x)
    in  x { sent = s }

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

instance (Ord t, Binary t) => Binary (Seg t) where
    put Seg{..} = do
        put orth
        put tags
        put oov 
    get = Seg <$> get <*> get <*> get

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
    deriving (Show, Eq, Ord, Binary)

-- | Make a weighted collection.  Negative elements will be ignored.
mkWMap :: Ord a => [(a, Double)] -> WMap a
mkWMap = WMap . M.fromListWith (+) . filter ((>=0).snd)

-- | Map function over weighted collection elements. 
mapWMap :: Ord b => (a -> b) -> WMap a -> WMap b
mapWMap f = mkWMap . map (first f) . M.toList . unWMap

--------------------------
-- Conversion
--------------------------

-- | Parse segment tags.
segTag :: P.Tagset -> Seg T.Text -> Seg P.Tag
segTag tagset = mapSeg (P.parseTag tagset)

-- | Show segment tags.
segText :: P.Tagset -> Seg P.Tag -> Seg T.Text
segText tagset = mapSeg (P.showTag tagset)

-- | Parse sentence tags.
sentTag :: P.Tagset -> Sent T.Text -> Sent P.Tag
sentTag = map . segTag

-- | Show sentence tags.
sentText :: P.Tagset -> Sent P.Tag -> Sent T.Text
sentText = map . segText

--------------------------------
-- Alignment and synchronization
--------------------------------

-- | Synchronize two datasets, taking disamb tags from the first one
-- and the rest of information form the second one.
sync :: P.Tagset -> [Seg P.Tag] -> [Seg P.Tag] -> [Seg P.Tag]
sync tagset xs ys = concatMap (uncurry (moveDisamb tagset)) (align xs ys)

-- | If both arguments contain only one segment, insert disamb interpretations
-- from the first segment into the second segment.  Otherwise, the first list
-- of segments will be returned unchanged.
moveDisamb :: P.Tagset -> [Seg P.Tag] -> [Seg P.Tag] -> [Seg P.Tag]
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
align :: [Seg t] -> [Seg t] -> [([Seg t], [Seg t])]
align [] [] = []
align [] _  = error "align: null xs, not null ys"
align _  [] = error "align: not null xs, null ys"
align xs ys =
    let (x, y) = match xs ys
        rest   = align (drop (length x) xs) (drop (length y) ys)
    in  (x, y) : rest

-- | Find the shortest, length-matching prefixes in the two input lists.
match :: [Seg t] -> [Seg t] -> ([Seg t], [Seg t])
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
