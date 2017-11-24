{-# LANGUAGE RecordWildCards #-}


-- | Accuracy statistics.


module NLP.Concraft.DAG.Morphosyntax.Accuracy
(
-- * Stats
  accuracy
, AccSel (..)
, AccCfg (..)
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
  }


-- | Accuracy selector.
data AccSel
  = All
    -- ^ All words
  | Oov
    -- ^ Only OOV words
  -- -- | NotOov
  deriving (Eq, Ord, Show)


goodAndBad
  :: Word w
  => AccCfg
  -> Sent w P.Tag
  -> Sent w P.Tag
  -> (Int, Int)
goodAndBad cfg dag1 dag2 =
    F.foldl' gather (0, 0) $ DAG.zipE dag1 dag2
  where
    gather stats (seg1, seg2)
      | accSel cfg == All || (accSel cfg == Oov && oov seg1) =
          gather0 stats (seg1, seg2)
      | otherwise = stats
    gather0 (good, bad) (seg1, seg2) =
      if consistent (choice cfg seg1) (choice cfg seg2)
      then (good + 1, bad)
      else (good, bad + 1)
    consistent xs ys
      | S.null xs && S.null ys = True
      | weakAcc cfg =
          (not . S.null)
          (S.intersection xs ys)
      | otherwise = xs == ys


goodAndBad'
  :: Word w
  => AccCfg
  -> [Sent w P.Tag]
  -> [Sent w P.Tag]
  -> (Int, Int)
goodAndBad' cfg data1 data2 =
    let add (g, b) (g', b') = (g + g', b + b')
    in  F.foldl' add (0, 0)
        [ goodAndBad cfg dag1 dag2
        | (dag1, dag2) <- zip data1 data2 ]


-- | Compute the accuracy of the model with respect to the labeled dataset.
accuracy :: Word w => AccCfg -> [Sent w P.Tag] -> [Sent w P.Tag] -> Double
accuracy cfg data1 data2 =
    let k = numCapabilities
        parts = partition k (zip data1 data2)
        xs = Par.parMap Par.rseq (uncurry (goodAndBad' cfg) . unzip) parts
        (good, bad) = F.foldl' add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)


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
