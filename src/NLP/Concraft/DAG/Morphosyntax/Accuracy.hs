{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


-- | Accuracy statistics.


module NLP.Concraft.DAG.Morphosyntax.Accuracy
(
-- * Stats
  Stats(..)
, AccSel (..)
, AccCfg (..)
, collect
, precision
, recall
) where


import           Prelude hiding (Word)
import           GHC.Conc (numCapabilities)

import qualified Control.Parallel as Par
import qualified Control.Parallel.Strategies as Par

import           Data.List (foldl', transpose)
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tagset.Positional as P

import qualified Data.DAG as DAG
import           NLP.Concraft.DAG.Morphosyntax
-- import           NLP.Concraft.DAG.Morphosyntax.Align

import Debug.Trace (trace)


-- -- | Add stats,
-- (.+.) :: Stats -> Stats -> Stats
-- Stats x y .+. Stats x' y' = Stats (x + x') (y + y')
--
--
-- -- | Accuracy given stats.
-- accuracy :: Stats -> Double
-- accuracy s
--     = fromIntegral (good s)
--     / fromIntegral (gold s)


-- -- | Accuracy weak lower bound.
-- weakLB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
-- weakLB tagset ref other =
--     foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
--   where
--     stats [x] [y]
--         | S.null (xTags `S.intersection` yTags) = Stats 0 1
--         | otherwise = Stats 1 1
--       where
--         xTags = choice tagset x
--         yTags = choice tagset y
--     stats xs _ = Stats 0 (length xs)
--
--
-- -- | Accuracy strong lower bound.
-- strongLB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
-- strongLB tagset ref other =
--     foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
--   where
--     stats [x] [y]
--         | xTags == yTags = Stats 1 1
--         | otherwise = Stats 0 1
--       where
--         xTags = choice tagset x
--         yTags = choice tagset y
--     stats xs _ = Stats 0 (length xs)
--
--
-- -- | Accuracy weak upper bound.
-- weakUB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
-- weakUB tagset ref other =
--     foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
--   where
--     stats [x] [y]
--         | S.null (xTags `S.intersection` yTags) = Stats 0 1
--         | otherwise = Stats 1 1
--       where
--         xTags = choice tagset x
--         yTags = choice tagset y
--     stats xs _ = Stats (length xs) (length xs)
--
--
-- -- | Accuracy strong upper bound.
-- strongUB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
-- strongUB tagset ref other =
--     foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
--   where
--     stats [x] [y]
--         | xTags == yTags = Stats 1 1
--         | otherwise = Stats 0 1
--       where
--         xTags = choice tagset x
--         yTags = choice tagset y
--     stats xs _ = Stats (length xs) (length xs)


-- | Configuration of accuracy computation.
data AccCfg = AccCfg
  { accSel    :: AccSel
    -- ^ Which segments should be taken into account
  , accTagset :: P.Tagset
    -- ^ The underlying tagset
  , expandTag :: Bool
    -- ^ Should the tags be expanded?
  , weakAcc :: Bool
    -- ^ If weak, there has to be an overlap in the tags assigned to a given
    -- segment in both datasets. Otherwise, the two sets of tags have to be
    -- identical.
  , discardProb0 :: Bool
    -- ^ Whether sentences with near 0 probability should be discarded from
    -- evaluation.
  }


-- | Accuracy selector.
data AccSel
  = All
    -- ^ All words
  | Oov
    -- ^ Only OOV words
  -- -- | NotOov
  deriving (Eq, Ord, Show)


-- | True positives, false positives, etc.
data Stats = Stats
  { tp :: !Int
  , fp :: !Int
  , tn :: !Int
  , fn :: !Int
  } deriving (Show, Eq, Ord)


-- | Initial statistics.
zeroStats = Stats 0 0 0 0


addStats :: Stats -> Stats -> Stats
addStats x y = Stats
  { tp = tp x + tp y
  , fp = fp x + fp y
  , tn = tn x + tn y
  , fn = fn x + fn y
  }


goodAndBad
  :: Word w
  => AccCfg
  -> Sent w P.Tag
  -> Sent w P.Tag
  -> Stats
goodAndBad cfg dag1 dag2
  | discardProb0 cfg && (dagProb dag1 < eps || dagProb dag2 < eps) = zeroStats
  | otherwise =
    -- By using `DAG.zipE'`, we allow the DAGs to be slighly different in terms
    -- of their edge sets.
      F.foldl' gather zeroStats $ DAG.zipE' dag1 dag2
  where
    eps = 1e-9

    gather stats (gold, tagg)
      | accSel cfg == All || (accSel cfg == Oov && isOov) =
          gather0 stats
          (maybe S.empty (choice cfg) gold)
          (maybe S.empty (choice cfg) tagg)
      | otherwise = stats
      where
        isOov = oov $ case (gold, tagg) of
          (Just seg, _) -> seg
          (_, Just seg) -> seg
          _ -> error "Accuracy.goodAndBad: impossible happened"

    gather0 stats gold tagg
      | S.null gold && S.null tagg =
          stats {tn = tn stats + 1}
      | S.null gold =
          stats {fp = fp stats + 1}
      | S.null tagg =
          stats {fn = fn stats + 1}
      | otherwise =
          if consistent gold tagg
          then stats {tp = tp stats + 1}
          else stats {fp = fp stats + 1, fn = fn stats + 1}

    consistent xs ys
      | weakAcc cfg = (not . S.null) (S.intersection xs ys)
      | otherwise = xs == ys


goodAndBad'
  :: Word w
  => AccCfg
  -> [Sent w P.Tag]
  -> [Sent w P.Tag]
  -> Stats
goodAndBad' cfg data1 data2 =
  F.foldl' addStats zeroStats
  [ goodAndBad cfg dag1 dag2
  | (dag1, dag2) <- zip data1 data2 ]


-- | Compute the accuracy of the model with respect to the labeled dataset.
collect :: Word w => AccCfg -> [Sent w P.Tag] -> [Sent w P.Tag] -> Stats
collect cfg data1 data2 =
    let k = numCapabilities
        parts = partition k (zip data1 data2)
        xs = Par.parMap Par.rseq (uncurry (goodAndBad' cfg) . unzip) parts
    in  F.foldl' addStats zeroStats xs
    -- in  fromIntegral good / fromIntegral (good + bad)


precision :: Stats -> Double
precision Stats{..}
  = fromIntegral tp
  / fromIntegral (tp + fp)


recall :: Stats -> Double
recall Stats{..}
  = fromIntegral tp
  / fromIntegral (tp + fn)


------------------------------------------------------
-- Verification
------------------------------------------------------


-- | Compute the probability of the DAG, based on the probabilities assigned to
-- different edges and their labels.
dagProb :: Sent w t -> Double
dagProb dag = sum
  [ fromEdge edgeID
  | edgeID <- DAG.dagEdges dag
  , DAG.isInitialEdge edgeID dag ]
  where
    fromEdge edgeID
      = edgeProb edgeID
      * fromNode (DAG.endsWith edgeID dag)
    edgeProb edgeID =
      let Seg{..} = DAG.edgeLabel edgeID dag
      in  sum . map snd . M.toList $ unWMap tags
    fromNode nodeID =
      case DAG.outgoingEdges nodeID dag of
        [] -> 1
        xs -> sum (map fromEdge xs)


-- -- | Filter out the sentences with ~0 probability.
-- verifyDataset :: [Sent w t] -> [Sent w t]
-- verifyDataset =
--   filter verify
--   where
--     verify dag = dagProb dag >= eps
--     eps = 1e-9


--------------------------
-- Utils
--------------------------


-- | All tags are expanded here.
choice :: AccCfg -> Seg w P.Tag -> S.Set P.Tag
choice AccCfg{..}
  = S.fromList . expandMaybe . best
  where
    expandMaybe
      | expandTag = concatMap (P.expand accTagset)
      | otherwise = id


-- -- | Positive tags.
-- positive :: Seg w t -> [t]
-- positive seg =
--     let xs = M.toList . unWMap . tags
--     in  [x | (x, v) <- xs seg, v > 0]


-- | The best tags.
best :: Seg w t -> [t]
best seg
  | null zs   = []
  | otherwise =
      let maxProb = maximum (map snd zs)
      in  if maxProb < eps
          then []
          else map fst
               . filter ((>= maxProb - eps) . snd)
               $ zs
  where
    zs = M.toList . unWMap . tags $ seg
    eps = 1.0e-9


partition :: Int -> [a] -> [[a]]
partition n =
    transpose . group n
  where
    group _ [] = []
    group k xs = take k xs : (group k $ drop k xs)
