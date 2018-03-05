{-# LANGUAGE RecordWildCards #-}


-- | Baseline word-segmentation functions.


module NLP.Concraft.DAG.Segmentation
( shortestPath
) where


import           Control.Monad (guard)

import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import           Data.Ord (comparing)

import           Data.DAG (DAG)
import qualified Data.DAG as DAG


------------------------------------
-- Baseline word-level segmentation
------------------------------------


-- | Select the shortest-path in the given DAG and remove all the edges which
-- are not on this path.
shortestPath :: DAG a b -> DAG a b
shortestPath dag = filterDAG (findShortestPath dag) dag


-- | Retrieve the nodes which belong to the shortest path in the given DAG.
findShortestPath :: DAG a b -> S.Set DAG.NodeID
findShortestPath dag
  = S.fromList . pick . map fst . reverse
  $ L.sortBy (comparing snd) first
  where
    first = do
      nodeID <- DAG.dagNodes dag
      guard . null $ DAG.ingoingEdges nodeID dag
      return (nodeID, dist nodeID)
    pick ids = case ids of
      nodeID : _ -> nodeID : forward nodeID
      [] -> error "Segmentation.findShortestPath: nothing to pick!?"
    forward nodeID
      | null (DAG.outgoingEdges nodeID dag) = []
      | otherwise = pick $ do
          nextEdgeID <- DAG.outgoingEdges nodeID dag
          let nextNodeID = DAG.endsWith nextEdgeID dag
          guard $ dist nodeID == dist nextNodeID - 1
          return nextNodeID
    dist = computeDist dag


-- | Compute the minimal distance from each node to a target node.
computeDist :: DAG a b -> DAG.NodeID -> Int
computeDist dag =
  dist
  where
    dist =
      Memo.wrap DAG.NodeID DAG.unNodeID Memo.integral dist'
    dist' nodeID
      | null (DAG.outgoingEdges nodeID dag) = 0
      | otherwise = minimum $ do
          nextEdgeID <- DAG.outgoingEdges nodeID dag
          let nextNodeID = DAG.endsWith nextEdgeID dag
          return $ dist nextNodeID + 1


-- | Remove the nodes (and the corresponding edges) which are not in the given set.
filterDAG :: S.Set DAG.NodeID -> DAG a b -> DAG a b
filterDAG nodeSet DAG.DAG{..} =
  DAG.DAG newNodeMap newEdgeMap
  where
    newNodeMap = M.fromList
      [ (nodeID, node)
      | (nodeID, node) <- M.toList nodeMap
      , nodeID `S.member` nodeSet ]
    newEdgeMap = M.fromList
      [ (edgeID, edge)
      | (edgeID, edge) <- M.toList edgeMap
      , DAG.tailNode edge `S.member` nodeSet
      , DAG.headNode edge `S.member` nodeSet ]
