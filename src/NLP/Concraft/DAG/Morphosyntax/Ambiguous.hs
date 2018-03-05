{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Segmentation-level ambiguities. TODO: consider moving the module contents
-- to `NLP.Concraft.DAG`.


module NLP.Concraft.DAG.Morphosyntax.Ambiguous
  ( identifyAmbiguousSegments
  ) where


import qualified Data.MemoCombinators as Memo
import qualified Data.DAG as DAG


------------------------------------------------------
-- Marking segmantation ambiguities
------------------------------------------------------


-- | Identify ambigouos segments (roughly, segments which can be by-passed) in
-- the given DAG. Such ambiguous edges are marked in the resulting DAG with
-- `True` values.
identifyAmbiguousSegments :: DAG.DAG a b -> DAG.DAG a Bool
identifyAmbiguousSegments dag =
  flip DAG.mapE dag $ \edgeID _ ->
    incoming edgeID * outgoing edgeID < totalPathNum
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
