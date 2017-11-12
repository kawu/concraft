{-# LANGUAGE RecordWildCards #-}


-- | Top-level module adated to DAGs, guessing and disambiguation.


module NLP.Concraft.DAGSeg
(
-- * Model
  Concraft (..)
, saveModel
, loadModel


-- * Annotation
, Anno
, replace

-- * Best paths
, findOptimalPaths
, disambPath

-- * Marginals
-- , D.ProbType (..)
, guessMarginals
, disambMarginals
, disambProbs

-- * Tagging
, guess
, guessSent
, tag
-- , tag'

-- * Training
, train

-- * Pruning
, prune
) where


import           System.IO (hClose)
import           Control.Applicative ((<$>), (<*>)) -- , (<|>))
import           Control.Arrow (first)
import           Control.Monad (when, guard)
-- import           Data.Maybe (listToMaybe)
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Binary (Binary, put, get, Put, Get)
import qualified Data.Binary as Binary
import           Data.Binary.Put (runPut)
import           Data.Binary.Get (runGet)
import           Data.Aeson
import qualified System.IO.Temp as Temp
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

import           Data.DAG (DAG, EdgeID)
import qualified Data.DAG as DAG

import qualified Data.Tagset.Positional as P

-- import           NLP.Concraft.Analysis
import           NLP.Concraft.Format.Temp
import qualified NLP.Concraft.DAG.Morphosyntax as X
import           NLP.Concraft.DAG.Morphosyntax (Sent, WMap)
import qualified NLP.Concraft.DAG.Guess as G
import qualified NLP.Concraft.DAG.DisambSeg as D


---------------------
-- Model
---------------------


modelVersion :: String
modelVersion = "dagseg:0.11"


-- | Concraft data.
data Concraft t = Concraft
  { tagset        :: P.Tagset
  , guessNum      :: Int
  , guesser       :: G.Guesser t P.Tag
  , disamb        :: D.Disamb t
  }


putModel :: (Ord t, Binary t) => Concraft t -> Put
putModel Concraft{..} = do
  put modelVersion
  put tagset
  put guessNum
  put guesser
  D.putDisamb disamb


-- | Get the model, given the tag simplification function for the disambigutation model.
getModel :: (Ord t, Binary t) => (P.Tagset -> t -> D.Tag) -> Get (Concraft t)
getModel smp = do
  comp <- get
  when (comp /= modelVersion) $ error $
    "Incompatible model version: " ++ comp ++
    ", expected: " ++ modelVersion
  tagset <- get
  Concraft tagset <$> get <*> get  <*> D.getDisamb (smp tagset)


-- | Save model in a file.  Data is compressed using the gzip format.
saveModel :: (Ord t, Binary t) => FilePath -> Concraft t -> IO ()
-- saveModel path = BL.writeFile path . GZip.compress . Binary.encode
saveModel path = BL.writeFile path . GZip.compress . runPut . putModel


-- | Load model from a file.
loadModel :: (Ord t, Binary t) => (P.Tagset -> t -> D.Tag) -> FilePath -> IO (Concraft t)
loadModel smp path = do
    -- x <- Binary.decode . GZip.decompress <$> BL.readFile path
    x <- runGet (getModel smp) . GZip.decompress <$> BL.readFile path
    x `seq` return x


----------------------
-- Annotation
----------------------


-- | DAG annotation, assignes @b@ values to @a@ labels for each edge in the
-- graph.
type Anno a b = DAG () (M.Map a b)


-- | Replace sentence probability values with the given annotation.
replace :: (Ord t) => Anno t Double -> Sent w t -> Sent w t
replace anno sent =
  fmap join $ DAG.zipE anno sent
  where
    join (m, seg) = seg {X.tags = X.fromMap m}
--     apply f
--       = X.fromMap
--       . M.mapWithKey (\key _val -> f M.! key)
--       . X.unWMap


-- | Extract marginal annotations from the given sentence.
extract :: Sent w t -> Anno t Double
extract = fmap $ X.unWMap . X.tags


----------------------
-- Best path
----------------------


-- | Find all optimal paths in the given annotation. Optimal paths are those
-- which go through tags with the assigned probability 1.
findOptimalPaths :: Anno t Double -> [[(EdgeID, t)]]
findOptimalPaths dag = do
  edgeID <- DAG.dagEdges dag
  guard $ DAG.isInitialEdge edgeID dag
  doit edgeID
  where
    doit i = inside i ++ final i
    inside i = do
      (tag, weight) <- M.toList (DAG.edgeLabel i dag)
      guard $ weight >= 1.0 - eps
      j <- DAG.nextEdges i dag
      xs <- doit j
      return $ (i, tag) : xs
    final i = do
      guard $ DAG.isFinalEdge i dag
      (tag, weight) <- M.toList (DAG.edgeLabel i dag)
      guard $ weight >= 1.0 - eps
      return [(i, tag)]
    eps = 1.0e-9


-- | Make the given path with disamb markers in the given annotation
-- and produce a new disamb annotation.
disambPath :: (Ord t) => [(EdgeID, t)] -> Anno t Double -> Anno t Bool
disambPath path =
  DAG.mapE doit
  where
    pathMap = M.fromList path
    doit edgeID m = M.fromList $ do
      let onPath = M.lookup edgeID pathMap
      x <- M.keys m
      return (x, Just x == onPath)


----------------------
-- Marginals and Probs
----------------------


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the guessing model.
guessMarginals :: (X.Word w, Ord t) => G.Guesser t P.Tag -> Sent w t -> Anno t Double
guessMarginals gsr = fmap X.unWMap . G.marginals gsr


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the guessing model.
disambMarginals :: (X.Word w, Ord t) => D.Disamb t -> Sent w t -> Anno t Double
-- disambMarginals dmb = fmap X.unWMap . D.marginals dmb
disambMarginals = disambProbs D.Marginals


-- | Determine probabilities corresponding to individual
-- tags w.r.t. the guessing model.
disambProbs :: (X.Word w, Ord t) => D.ProbType -> D.Disamb t -> Sent w t -> Anno t Double
disambProbs typ dmb = fmap X.unWMap . D.probs typ dmb


-------------------------------------------------
-- Trimming
-------------------------------------------------


-- | Trim down the set of potential labels to `k` most probable ones
-- for each OOV word in the sentence.
trimOOV :: (X.Word w, Ord t) => Int -> Sent w t -> Sent w t
trimOOV k =
  fmap trim
  where
    trim edge = if X.oov edge
      then trimEdge edge
      else edge
    trimEdge edge = edge {X.tags = X.trim k (X.tags edge)}


---------------------
-- Tagging
---------------------


-- | Determine marginal probabilities corresponding to individual tags w.r.t.
-- the guessing model and, afterwards, trim the sentence to keep only the `k`
-- most probably labels for each OOV edge. Note that, for OOV words, the entire
-- set of default tags is considered.
guessSent :: (X.Word w, Ord t) => Int -> G.Guesser t P.Tag -> Sent w t -> Sent w t
guessSent k gsr sent = trimOOV k $ replace (guessMarginals gsr sent) sent


-- | Perform guessing, trimming, and finally determine marginal probabilities
-- corresponding to individual tags w.r.t. the guessing model.
guess :: (X.Word w, Ord t) => Int -> G.Guesser t P.Tag -> Sent w t -> Anno t Double
guess k gsr = extract . guessSent k gsr


-- | Perform guessing, trimming, and finally determine marginal probabilities
-- corresponding to individual tags w.r.t. the disambiguation model.
tag :: (X.Word w, Ord t) => Int -> Concraft t -> Sent w t -> Anno t Double
tag k crf = disambMarginals (disamb crf) . guessSent k (guesser crf)


-- -- | Perform guessing, trimming, and finally determine probabilities
-- -- corresponding to individual tags w.r.t. the disambiguation model.
-- tag' :: X.Word w => Int -> D.ProbType -> Concraft -> Sent w P.Tag -> Anno P.Tag Double
-- tag' k typ Concraft{..} = disambProbs typ disamb . guessSent k guesser


---------------------
-- Training
---------------------


-- | Train the `Concraft` model.
-- No reanalysis of the input data will be performed.
--
-- The `FromJSON` and `ToJSON` instances are used to store processed
-- input data in temporary files on a disk.
train
    :: (X.Word w, Ord t)
    => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
                            --   of the training and evaluation input data
                            --   must correspond.
    -> Int                  -- ^ How many tags is the guessing model supposed
                            --   to produce for a given OOV word?  It will be
                            --   used (see `G.guessSent`) on both training and
                            --   evaluation input data prior to the training
                            --   of the disambiguation model.
    -> G.TrainConf t P.Tag  -- ^ Training configuration for the guessing model.
    -> D.TrainConf t        -- ^ Training configuration for the
                            --   disambiguation model.
    -> IO [Sent w t]    -- ^ Training dataset.  This IO action will be
                            --   executed a couple of times, so consider using
                            --   lazy IO if your dataset is big.
    -> IO [Sent w t]    -- ^ Evaluation dataset IO action.  Consider using
                            --   lazy IO if your dataset is big.
    -> IO (Concraft t)
train tagset guessNum guessConf disambConf trainR'IO evalR'IO = do
  Temp.withTempDirectory "." ".guessed" $ \tmpDir -> do
  let temp = withTemp tagset tmpDir

  putStrLn "\n===== Train guessing model ====="
  guesser <- G.train guessConf trainR'IO evalR'IO
  let guess = guessSent guessNum guesser
  trainG  <- map guess <$> trainR'IO
  evalG   <- map guess <$> evalR'IO

  temp "train" trainG $ \trainG'IO -> do
  temp "eval"  evalG  $ \evalG'IO  -> do

  putStrLn "\n===== Train disambiguation model ====="
  disamb <- D.train disambConf trainG'IO evalG'IO
  return $ Concraft tagset guessNum guesser disamb


---------------------
-- Temporary storage
---------------------


-- | Store dataset on a disk and run a handler on a list which is read
-- lazily from the disk.  A temporary file will be automatically
-- deleted after the handler is done.
--
-- NOTE: (11/11/2017): it's just a dummy function right now, which does
-- not use disk storage at all.
--
withTemp
  -- :: (FromJSON w, ToJSON w)
  :: P.Tagset
  -> FilePath                     -- ^ Directory to create the file in
  -> String                       -- ^ Template for `Temp.withTempFile`
  -> [Sent w t]                   -- ^ Input dataset
  -> (IO [Sent w t] -> IO a)      -- ^ Handler
  -> IO a
withTemp _      _   _    [] handler = handler (return [])
withTemp tagset dir tmpl xs handler =
  Temp.withTempFile dir tmpl $ \tmpPath tmpHandle -> do
    hClose tmpHandle
    let txtSent = X.mapSent $ P.showTag tagset
        tagSent = X.mapSent $ P.parseTag tagset
    handler (return xs)


---------------------
-- Pruning
---------------------


-- | Prune disambiguation model: discard model features with
-- absolute values (in log-domain) lower than the given threshold.
prune :: Double -> Concraft t -> Concraft t
prune x concraft =
    let disamb' = D.prune x (disamb concraft)
    in  concraft { disamb = disamb' }
