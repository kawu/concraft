{-# LANGUAGE RecordWildCards #-}


-- | Baseline word-segmentation functions.


module NLP.Concraft.DAG.Segmentation
( PathTyp (..)
, pickPath
, findPath

-- * Frequencies
, computeFreqs
, FreqConf (..)
) where


import           Control.Monad (guard)
-- import qualified Control.Monad.State.Strict as State

import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Ord (comparing)

import           Data.DAG (DAG)
import qualified Data.DAG as DAG

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Morphosyntax.Ambiguous as Ambi


------------------------------------
-- Shortest-path segmentation
------------------------------------

-- | Configuration related to frequency-based path picking.
data FreqConf = FreqConf
  { pickFreqMap :: M.Map T.Text (Int, Int)
    -- ^ A map which assigns (chosen, not chosen) counts to the invidiaul
    -- orthographic forms (see `computeFreqs`).
  , smoothingParam :: Double
    -- ^ A naive smoothing related parameter, which should be adddd to each
    -- count in `pickFreqMap`.
--   , orth :: DAG.EdgeID -> T.Text
--     -- ^ Orthographic form of a given edge
  }


-- | Which path type to search: shortest (`Min`) or longest (`Max`)
data PathTyp
  = Min
  | Max
  | Freq FreqConf


-- | Select the shortest-path (or longest, depending on `PathTyp`) in the given
-- DAG and remove all the edges which are not on this path.
pickPath
  :: (X.Word b)
  => PathTyp
  -> DAG a b
  -> DAG a b
pickPath pathTyp dag =
  let
    dag' = DAG.filterDAG (findPath pathTyp dag) dag
  in
    if DAG.isOK dag'
    then dag'
    else error "Segmentation.pickPath: the resulting DAG not correct"


-- | Retrieve the edges which belong to the shortest/longest (depending on the
-- argument function: `minimum` or `maximum`) path in the given DAG.
findPath
  :: (X.Word b)
  => PathTyp
  -> DAG a b
  -> S.Set DAG.EdgeID
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
      Max -> reverse
      _ -> id
    forward nodeID
      | null (DAG.outgoingEdges nodeID dag) = []
      | otherwise = pick $ do
          nextEdgeID <- DAG.outgoingEdges nodeID dag
          let nextNodeID = DAG.endsWith nextEdgeID dag
          -- guard $ dist nodeID == dist nextNodeID + 1
          guard $ dist nodeID == dist nextNodeID + arcLen nextEdgeID
          -- return nextNodeID
          return nextEdgeID
    pickNode ids = case ids of
      nodeID : _ -> forward nodeID
      [] -> error "Segmentation.pickPath: no node to pick!?"
    pick ids = case ids of
      edgeID : _ -> edgeID : forward (DAG.endsWith edgeID dag)
      [] -> error "Segmentation.pickPath: nothing to pick!?"
    dist = computeDist pathTyp dag
    -- distance between two nodes connected by an arc
    arcLen =
      case pathTyp of
        Freq conf -> computeArcLen conf dag
        _ -> const 1


------------------------------------
-- Distance from target nodes
------------------------------------


-- | Compute the minimal/maximal distance (depending on the argument function)
-- from each node to a target node.
computeDist
  :: (X.Word b)
  => PathTyp
  -> DAG a b
  -> DAG.NodeID
  -> Double
computeDist pathTyp dag =
  dist
  where
    minMax = case pathTyp of
      Max -> maximum
      _ -> minimum
    dist =
      Memo.wrap DAG.NodeID DAG.unNodeID Memo.integral dist'
    dist' nodeID
      | null (DAG.outgoingEdges nodeID dag) = 0
      | otherwise = minMax $ do
          nextEdgeID <- DAG.outgoingEdges nodeID dag
          let nextNodeID = DAG.endsWith nextEdgeID dag
          -- return $ dist nextNodeID + 1
          return $ dist nextNodeID + arcLen nextEdgeID
    arcLen =
      case pathTyp of
        Freq conf -> computeArcLen conf dag
        _ -> const 1


------------------------------------
-- Frequency-based segmentation
------------------------------------


-- | Compute chosen/not-chosen counts of the individual orthographic forms in
-- the DAGs. Only the ambiguous segments are taken into account.
computeFreqs :: (X.Word w) => [X.Sent w t] -> M.Map T.Text (Int, Int)
computeFreqs dags = M.fromListWith addBoth $ do
  dag <- dags
  let ambiDAG = Ambi.identifyAmbiguousSegments dag
  edgeID <- DAG.dagEdges dag
  guard $ DAG.edgeLabel edgeID ambiDAG == True
  let seg = DAG.edgeLabel edgeID dag
      orth = edgeOrth seg
      edgeWeight = sum . M.elems . X.unWMap . X.tags $ seg
      eps = 1e-9
  return $
    if edgeWeight > eps
    then (orth, (1, 0))
    else (orth, (0, 1))
  where
    addBoth (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


computeArcLen
  :: (X.Word b)
  => FreqConf
  -> DAG a b
  -> DAG.EdgeID
  -> Double
computeArcLen FreqConf{..} dag edgeID =
  (\x -> -x) . log $
    case M.lookup (edgeOrth $ DAG.edgeLabel edgeID dag) pickFreqMap of
      Just (chosen, notChosen) ->
        (fromIntegral chosen + smoothingParam) /
        (fromIntegral (chosen + notChosen) + smoothingParam*2)
      Nothing -> 0.5 -- smoothingParam / (smoothingParam*2)


-- | Retrieve the orthographic representation of a given segment for the purpose
-- of frequency-based segmentation.
edgeOrth :: X.Word w => w -> T.Text
edgeOrth = T.toLower . T.strip . X.orth


------------------------------------
-- Frequency-based segmentation
--
-- How this can work?
--
-- For each segment (i.e, a particular orthographic form) we would like to find
-- a simple measure of how likely it is to use it in a segmentation.
--
-- # Solution 1
--
-- A simple way would be to determine the probability as follows:
--
--   p(orth) = chosen(orth) / possible(orth)
--
-- where `chosen(orth)` is the number of *chosen* (disamb) edges in the training
-- dataset whose orthographic form is `orth`, and `possible(orth)` is the total
-- number of edges in train with the `orth` orthographic form.
--
-- Now, the problem is that we would need to use smoothing to account for forms
-- not in the training dataset:
--
--   p(orth) = chosen(orth) + 1 / possible(orth) + 2
--
-- The reason to add 2 in the denominator is that it can be rewritten as:
--
--   p(orth) = chosen(orth) + 1 / chosen(orth) + 1 + not-chosen(orth) + 1
--
-- So the default probability is 1/2.  Not too bad?
--
-- # Solution 2
--
-- An alternative would be to decide, for a given segment, whether it should be
-- taken or not. For example, if a given segment (i.e., orthographic form) is
-- chosen in more than a half of situations where it can actually be chosen,
-- then it should belong to the path.  Otherwise, it should not.
--
-- Then we have to choose how to represent the fact that the edge should be
-- taken (i.e. should belong to a path). One way to do that is to say that, if
-- the form is chosen, its weight is 0; otherwise, its weight is 1. This does
-- not account for the length of edges, so another solution would be to say that
-- if the edge/form is chosen, then its weight is 0; otherwise, it is equal to
-- its length. Then again, the length of an edge can be computed in several
-- manners, e.g., as the string length of the orthographic form, or as the
-- number of segments which can be used inside. But the latter is not always
-- possible to compute.
--
-- # Choice
--
-- For now, solution 1 seems more principled. So we need to compute a map from
-- orthographic forms to pairs of (chosen, not chosen) counts on the basis of
-- the training dataset. Afterwards, we use "naive" smoothing
-- (http://ivan-titov.org/teaching/nlmi-15/lecture-4.pdf) and transform the
-- resulting probability with `(-) . log`. This gives as a positive value
-- assigned to each segment, and we need to find the path with the lowest
-- weigth.
------------------------------------
