{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


-- | Accuracy statistics.


module NLP.Concraft.DAG.Morphosyntax.Accuracy
(
-- * Stats
  Stats(..)
, AccCfg (..)
, collect
, precision
, recall
, accuracy
) where


import           Prelude hiding (Word)
import           GHC.Conc (numCapabilities)

import           Control.Arrow (first)
import qualified Control.Parallel.Strategies as Par

import           Data.List (transpose)
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Tagset.Positional as P

import qualified Data.DAG as DAG
import           NLP.Concraft.DAG.Morphosyntax
import           NLP.Concraft.DAG.Morphosyntax.Ambiguous
  (identifyAmbiguousSegments)
-- import           NLP.Concraft.DAG.Morphosyntax.Align

-- import qualified Data.Text as T
import Debug.Trace (trace)


-- | Configuration of accuracy computation.
data AccCfg x = AccCfg
  { onlyOov   :: Bool
    -- ^ Limit calculations to OOV words
  , onlyAmb   :: Bool
    -- ^ Limit calculations to segmentation-ambiguous words
  , onlyMarkedWith :: S.Set x
    -- ^ Limit calculations to segments marked with one of the given labels;
    -- if empty, the option has no effect
  , accTagset :: P.Tagset
    -- ^ The underlying tagset
  , expandTag :: Bool
    -- ^ Should the tags be expanded?
  , ignoreTag :: Bool
    -- ^ Compute segmentation-level accurracy. The actually chosen tags are
    -- ignored, only information about the chosen DAG edges is relevant.
  , weakAcc :: Bool
    -- ^ If weak, there has to be an overlap in the tags assigned to a given
    -- segment in both datasets. Otherwise, the two sets of tags have to be
    -- identical.
--   , discardProb0 :: Bool
--     -- ^ Whether sentences with near 0 probability should be discarded from
--     -- evaluation.
  , verbose :: Bool
    -- ^ Print information about compared elements
  }


-- | True positives, false positives, etc.
data Stats = Stats
  { tp :: !Int
    -- ^ True positive
  , fp :: !Int
    -- ^ False positive
  , tn :: !Int
    -- ^ True negative
  , fn :: !Int
    -- ^ False negative
  , ce :: !Int
    -- ^ Consistency error (number of edges for which both `fp` and `fn` hold)
  } deriving (Show, Eq, Ord)


-- | Initial statistics.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0


addStats :: Stats -> Stats -> Stats
addStats x y = Stats
  { tp = tp x + tp y
  , fp = fp x + fp y
  , tn = tn x + tn y
  , fn = fn x + fn y
  , ce = ce x + ce y
  }


goodAndBad
  :: (Word w, Ord x, Show x)
  => AccCfg x
  -> Sent w (P.Tag, x) -- ^ Gold (reference) DAG
  -> Sent w (P.Tag, x) -- ^ Tagged (to compare) DAG
  -> Stats
goodAndBad cfg dag1 dag2 =
--   | discardProb0 cfg && (dagProb dag1 < eps || dagProb dag2 < eps) = zeroStats
--   | otherwise =
  -- By using `DAG.zipE'`, we allow the DAGs to be slighly different in terms
  -- of their edge sets.
  F.foldl' addStats zeroStats
  . DAG.mapE gather
  $ dag
  where
    eps = 1e-9

    dag = DAG.zipE' dag1 dag2
    ambiDag = identifyAmbiguousSegments dag

    traceThem gold tagg =
      if verbose cfg
      then trace
           ( let info = (,) <$> orth <*> choice cfg in
               "comparing '" ++
               show (info <$> gold) ++
               "' with '" ++
               show (info <$> tagg) ++
               "'"
           )
      else id

    gather edgeID (gold, tagg)
      | (onlyOov cfg `implies` isOov) &&
        (onlyAmb cfg `implies` isAmb) &&
        ((not . S.null) (onlyMarkedWith cfg) `implies` isMarked) =
          traceThem gold tagg $
          gather0
          (maybe S.empty (choice cfg) gold)
          (maybe S.empty (choice cfg) tagg)
      | otherwise = zeroStats
      where
        isOov = oov $ case (gold, tagg) of
          (Just seg, _) -> seg
          (_, Just seg) -> seg
          _ -> error "Accuracy.goodAndBad: impossible happened"
        hasMarker =
          any (`S.member` onlyMarkedWith cfg) . map (snd . fst) . M.toList
        isMarked = hasMarker $ case (gold, tagg) of
          (Just seg1, Just seg2) ->
            unWMap (tags seg1) `M.union` unWMap (tags seg2)
          (Just seg, _) -> unWMap $ tags seg
          (_, Just seg) -> unWMap $ tags seg
          _ -> error "Accuracy.goodAndBad: impossible2 happened"
        isAmb = DAG.edgeLabel edgeID ambiDag

    gather0 gold tagg
      | S.null gold && S.null tagg =
          zeroStats {tn = 1}
      | S.null gold =
          zeroStats {fp = 1}
      | S.null tagg =
          zeroStats {fn = 1}
      | otherwise =
          if consistent gold tagg
          then zeroStats {tp = 1}
          else zeroStats {fp = 1, fn = 1, ce = 1}

    consistent xs ys
      | weakAcc cfg = (not . S.null) (S.intersection xs ys)
      | otherwise = xs == ys


goodAndBad'
  :: (Word w, Ord x, Show x)
  => AccCfg x
  -> [Sent w (P.Tag, x)]
  -> [Sent w (P.Tag, x)]
  -> Stats
goodAndBad' cfg goldData taggData =
  F.foldl' addStats zeroStats
  [ goodAndBad cfg dag1 dag2
  | (dag1, dag2) <- zip goldData taggData ]


-- | Compute the accuracy of the model with respect to the labeled dataset.
-- To each `P.Tag` an additional information `x` can be assigned, which will be
-- taken into account when computing statistics.
collect
  :: (Word w, Ord x, Show x)
  => AccCfg x
  -> [Sent w (P.Tag, x)] -- ^ Gold dataset
  -> [Sent w (P.Tag, x)] -- ^ Tagged dataset (to be compare with the gold)
  -> Stats
collect cfg goldData taggData =
    let k = numCapabilities
        parts = partition k (zip goldData taggData)
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


accuracy :: Stats -> Double
accuracy Stats{..}
  = fromIntegral (tp + tn)
  / fromIntegral (tp + fp + tn + fn - ce)
  -- Not that, above, we substract `ce` so as to count inconsistency errors
  -- as single ones (their are accounted for twice in `fp + fn`).


------------------------------------------------------
-- Verification
------------------------------------------------------


-- -- | Compute the probability of the DAG, based on the probabilities assigned to
-- -- different edges and their labels.
-- WARNING: the `dagProb` is most likely not correct, see the `crf-chain1-constrained`
-- library for a correct version.
-- dagProb :: Sent w t -> Double
-- dagProb dag = sum
--   [ fromEdge edgeID
--   | edgeID <- DAG.dagEdges dag
--   , DAG.isInitialEdge edgeID dag ]
--   where
--     fromEdge edgeID
--       = edgeProb edgeID
--       * fromNode (DAG.endsWith edgeID dag)
--     edgeProb edgeID =
--       let Seg{..} = DAG.edgeLabel edgeID dag
--       in  sum . map snd . M.toList $ unWMap tags
--     fromNode nodeID =
--       case DAG.outgoingEdges nodeID dag of
--         [] -> 1
--         xs -> sum (map fromEdge xs)


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


-- | Select the chosen tags.
--
--   * Tag expansion is performed here (if demanded)
--   * Tags are replaced by a dummy in case of `AmbiSeg` comparison
choice :: (Ord x) => AccCfg x -> Seg w (P.Tag, x) -> S.Set (P.Tag, x)
choice AccCfg{..}
  = S.fromList . expandMaybe . best
  where
    expandMaybe
      | ignoreTag = map (first $ const dummyTag)
      | expandTag = concatMap (\(tag, x) -> map (,x) $ P.expand accTagset tag)
      | otherwise = id
    dummyTag = P.Tag "AmbiSeg" M.empty


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


-- | Implication.
implies :: Bool -> Bool -> Bool
implies p q = if p then q else True
