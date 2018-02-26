{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Accuracy statistics.


module NLP.Concraft.DAG.Morphosyntax.Accuracy
(
-- * Stats
  Stats(..)
, AccCfg (..)
, collect
, precision
, recall
) where


import           Prelude hiding (Word)
import           GHC.Conc (numCapabilities)

import qualified Control.Parallel.Strategies as Par
import qualified Data.MemoCombinators as Memo

import           Data.List (transpose)
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Tagset.Positional as P

import qualified Data.DAG as DAG
import           NLP.Concraft.DAG.Morphosyntax
-- import           NLP.Concraft.DAG.Morphosyntax.Align

import qualified Data.Text as T
import Debug.Trace (trace)


-- | Configuration of accuracy computation.
data AccCfg = AccCfg
  { onlyOov   :: Bool
    -- ^ Limit calculations to OOV words
  , onlyAmb   :: Bool
    -- ^ Limit calculations to segmentation-ambiguous words
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
  , discardProb0 :: Bool
    -- ^ Whether sentences with near 0 probability should be discarded from
    -- evaluation.
  }


-- | True positives, false positives, etc.
data Stats = Stats
  { tp :: !Int
  , fp :: !Int
  , tn :: !Int
  , fn :: !Int
  } deriving (Show, Eq, Ord)


-- | Initial statistics.
zeroStats :: Stats
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
  -> Sent w P.Tag -- ^ Gold (reference) DAG
  -> Sent w P.Tag -- ^ Tagged (to compare) DAG
  -> Stats
goodAndBad cfg dag1 dag2
  | discardProb0 cfg && (dagProb dag1 < eps || dagProb dag2 < eps) = zeroStats
  | otherwise =
    -- By using `DAG.zipE'`, we allow the DAGs to be slighly different in terms
    -- of their edge sets.
      F.foldl' addStats zeroStats
      . DAG.mapE gather
      $ dag
  where
    eps = 1e-9

    dag = DAG.zipE' dag1 dag2
    ambiDag = identifyAmbiguousSegments dag

    gather edgeID (gold, tagg)
      | (onlyOov cfg `implies` isOov) &&
        (onlyAmb cfg `implies` isAmb) =
          trace ("comparing '" ++ show (orth <$> gold) ++ "' with '" ++ show (orth <$> tagg) ++ "'") $
          gather0
          (maybe S.empty (choice cfg) gold)
          (maybe S.empty (choice cfg) tagg)
      | otherwise = zeroStats
      where
        isOov = oov $ case (gold, tagg) of
          (Just seg, _) -> seg
          (_, Just seg) -> seg
          _ -> error "Accuracy.goodAndBad: impossible happened"
        isAmb = DAG.edgeLabel edgeID ambiDag

    gather0 gold tagg
      | S.null gold && S.null tagg =
          zeroStats {tn = 1}
      | S.null gold =
          zeroStats {fp = 1}
      | S.null tagg =
          zeroStats {fn = 0 + 1}
      | otherwise =
          if consistent gold tagg
          then zeroStats {tp = 1}
          else zeroStats {fp = 1, fn = 1}

    consistent xs ys
      | weakAcc cfg = (not . S.null) (S.intersection xs ys)
      | otherwise = xs == ys


goodAndBad'
  :: Word w
  => AccCfg
  -> [Sent w P.Tag]
  -> [Sent w P.Tag]
  -> Stats
goodAndBad' cfg goldData taggData =
  F.foldl' addStats zeroStats
  [ goodAndBad cfg dag1 dag2
  | (dag1, dag2) <- zip goldData taggData ]


-- | Compute the accuracy of the model with respect to the labeled dataset.
collect
  :: Word w
  => AccCfg
  -> [Sent w P.Tag] -- ^ Gold dataset
  -> [Sent w P.Tag] -- ^ Tagged dataset (to be compare with the gold)
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


------------------------------------------------------
-- Marking segmantation ambiguities
------------------------------------------------------


-- | Identify ambigouos segments (roughly, segments which can be by-passed) in
-- the given DAG. Such ambiguous edges are marked in the resulting DAG with
-- `True` values.
identifyAmbiguousSegments :: DAG.DAG a b -> DAG.DAG a Bool
identifyAmbiguousSegments dag =
  flip DAG.mapE dag $ \edgeID _ ->
    -- incoming edgeID * outgoing edgeID < totalPathNum
    False
  where
    incoming = inComingNum dag
    outgoing = outGoingNum dag
    totalPathNum = sum
      [ outgoing edgeID
      | edgeID <- DAG.dagEdges dag
      , DAG.isInitialEdge edgeID dag ]


-- | Compute the number of paths from a starting edge to the given edge.
inComingNum :: DAG.DAG a b -> DAG.EdgeID -> Int
inComingNum dag =
  incoming
  where
    incoming =
      Memo.wrap DAG.EdgeID DAG.unEdgeID Memo.integral incoming'
    incoming' edgeID
      | DAG.isInitialEdge edgeID dag = 1
      | otherwise = sum $ do
          prevID <- DAG.prevEdges edgeID dag
          return $ incoming prevID


-- | Compute the number of paths from the given edge to a target edge.
outGoingNum :: DAG.DAG a b -> DAG.EdgeID -> Int
outGoingNum dag =
  outgoing
  where
    outgoing =
      Memo.wrap DAG.EdgeID DAG.unEdgeID Memo.integral outgoing'
    outgoing' edgeID
      | DAG.isFinalEdge edgeID dag = 1
      | otherwise = sum $ do
          nextID <- DAG.nextEdges edgeID dag
          return $ outgoing nextID


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


-- | Select the chosen tags.
--
--   * Tag expansion is performed here (if demanded)
--   * Tags are replaced by a dummy in case of `AmbiSeg` comparison
choice :: AccCfg -> Seg w P.Tag -> S.Set P.Tag
choice AccCfg{..}
  = S.fromList . expandMaybe . best
  where
    expandMaybe
      | ignoreTag = map (const dummyTag)
      | expandTag = concatMap (P.expand accTagset)
      | otherwise = id
    dummyTag = P.Tag "AmbiSeg" M.empty


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


-- | Implication.
implies :: Bool -> Bool -> Bool
implies p q = if p then q else True
