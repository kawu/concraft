{-# LANGUAGE RecordWildCards #-}


-- | Baseline word-segmentation functions.


module NLP.Concraft.DAG.Segmentation
( PathTyp (..)
, pickPath
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
-- Shortest-path segmentation
------------------------------------


-- | Which path type to search: shortest (`Min`) or longest (`Max`)
data PathTyp = Min | Max
  deriving (Show, Eq, Ord)


-- | Select the shortest-path in the given DAG and remove all the edges which
-- are not on this path.
pickPath :: PathTyp -> DAG a b -> DAG a b
pickPath pathTyp dag =
  let
    dag' = DAG.filterDAG (findPath pathTyp dag) dag
  in
    if DAG.isOK dag'
    then dag'
    else error "Segmentation.pickPath: the resulting DAG not correct"


-- | Retrieve the edges which belong to the shortest/longest (depending on the
-- argument function: `minimum` or `maximum`) path in the given DAG.
findPath :: PathTyp -> DAG a b -> S.Set DAG.EdgeID
findPath pathTyp dag
  = S.fromList . pickNode . map fst
  -- Below, we take the node with the smallest (reverse) or highest (no reverse)
  -- distance to a target node, depending on the path type (`Min` or `Max`).
  . reverseOrNot
  . L.sortBy (comparing snd)
  $ sourceNodes
  where
    sourceNodes = do
      nodeID <- DAG.dagNodes dag
      guard . null $ DAG.ingoingEdges nodeID dag
      return (nodeID, dist nodeID)
    reverseOrNot = case pathTyp of
      Min -> id
      Max -> reverse
    forward nodeID
      | null (DAG.outgoingEdges nodeID dag) = []
      | otherwise = pick $ do
          nextEdgeID <- DAG.outgoingEdges nodeID dag
          let nextNodeID = DAG.endsWith nextEdgeID dag
          guard $ dist nodeID == dist nextNodeID + 1
          -- return nextNodeID
          return nextEdgeID
    pickNode ids = case ids of
      nodeID : _ -> forward nodeID
      [] -> error "Segmentation.pickPath: no node to pick!?"
    pick ids = case ids of
      edgeID : _ -> edgeID : forward (DAG.endsWith edgeID dag)
      [] -> error "Segmentation.pickPath: nothing to pick!?"
    dist = computeDist pathTyp dag


------------------------------------
-- Distance from target nodes
------------------------------------


-- | Compute the minimal/maximal distance (depending on the argument function)
-- from each node to a target node.
computeDist :: PathTyp -> DAG a b -> DAG.NodeID -> Int
computeDist pathTyp dag =
  dist
  where
    minMax = case pathTyp of
      Min -> minimum
      Max -> maximum
    dist =
      Memo.wrap DAG.NodeID DAG.unNodeID Memo.integral dist'
    dist' nodeID
      | null (DAG.outgoingEdges nodeID dag) = 0
      | otherwise = minMax $ do
          nextEdgeID <- DAG.outgoingEdges nodeID dag
          let nextNodeID = DAG.endsWith nextEdgeID dag
          return $ dist nextNodeID + 1
