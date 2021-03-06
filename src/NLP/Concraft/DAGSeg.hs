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

-- * Disambiguation / best paths
, disamb
, findOptimalPaths
, disambPath

-- * Marginals
-- , D.ProbType (..)
, guessMarginals
, disambMarginals
, disambProbs

-- * Tagging
, guessSent
, guess
, tag
-- , tag'

-- -- * Training
-- , train

-- * Pruning
, prune
) where


-- import           Prelude hiding (Word)
-- import           System.IO (hClose)
import           Control.Applicative ((<$>), (<*>)) -- , (<|>))
import           Control.Arrow (second)
import           Control.Monad (when, guard)
-- import           Data.Maybe (listToMaybe)
-- import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Binary (Binary, put, get, Put, Get)
-- import qualified Data.Binary as Binary
import           Data.Binary.Put (runPut)
import           Data.Binary.Get (runGet)
-- import           Data.Aeson
-- import qualified System.IO.Temp as Temp
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip
import           Data.Ord (comparing)
import           Data.List (sortBy)

import           Data.DAG (DAG, EdgeID)
import qualified Data.DAG as DAG

import qualified Data.Tagset.Positional as P

import qualified Data.CRF.Chain1.Constrained.DAG as CRF

-- import           NLP.Concraft.Analysis
-- import           NLP.Concraft.Format.Temp
import qualified NLP.Concraft.DAG.Morphosyntax as X
import           NLP.Concraft.DAG.Morphosyntax (Sent)
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
  , segmenter     :: D.Disamb t
  , disamber        :: D.Disamb t
  }


putModel :: (Ord t, Binary t) => Concraft t -> Put
putModel Concraft{..} = do
  put modelVersion
  put tagset
  put guessNum
  G.putGuesser guesser
  D.putDisamb segmenter
  D.putDisamb disamber


-- | Get the model, given the tag simplification function for the disambigutation model.
getModel
  :: (Ord t, Binary t)
  => (P.Tagset -> t -> P.Tag)
     -- ^ Guesser simplification function
  -> (P.Tagset -> P.Tag -> t)
     -- ^ Guesser complexification function
  -> (P.Tagset -> t -> D.Tag)
     -- ^ Segmentation/disamb simplification function (TODO: two different
     -- simplification functions?)
  -> Get (Concraft t)
getModel gsrSmp gsrCpx dmbSmp = do
  comp <- get
  when (comp /= modelVersion) $ error $
    "Incompatible model version: " ++ comp ++
    ", expected: " ++ modelVersion
  tagset <- get
  Concraft tagset <$> get
    <*> G.getGuesser (gsrSmp tagset) (gsrCpx tagset)
    <*> D.getDisamb (dmbSmp tagset)
    <*> D.getDisamb (dmbSmp tagset)


-- | Save model in a file.  Data is compressed using the gzip format.
saveModel :: (Ord t, Binary t) => FilePath -> Concraft t -> IO ()
-- saveModel path = BL.writeFile path . GZip.compress . Binary.encode
saveModel path = BL.writeFile path . GZip.compress . runPut . putModel


-- | Load model from a file.
loadModel
  :: (Ord t, Binary t)
  => (P.Tagset -> t -> P.Tag)
     -- ^ Guesser simplification function
  -> (P.Tagset -> P.Tag -> t)
     -- ^ Guesser complexification function
  -> (P.Tagset -> t -> D.Tag)
     -- ^ Disamb simplification function
  -> FilePath
  -> IO (Concraft t)
loadModel gsrSmp gsrCpx dmbSmp path = do
    -- x <- Binary.decode . GZip.decompress <$> BL.readFile path
    x <- runGet (getModel gsrSmp gsrCpx dmbSmp) . GZip.decompress <$> BL.readFile path
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


-- | Insert the guessing results into the sentence. Only interpretations of OOV
-- words will be extended.  The probabilities of the new tags are set to 0.
insertGuessed :: (X.Word w, Ord t) => Anno t Double -> Sent w t -> Sent w t
insertGuessed anno sent =
  fmap join $ DAG.zipE anno sent
  where
    join (gueMap, seg)
      | X.oov (X.word seg) =
          let oldMap  = X.unWMap (X.tags seg)
              gueMap0 = M.fromList
                      . map (second $ const 0)
                      $ M.toList gueMap
              newMap  = M.unionWith (+) oldMap gueMap0
          in  seg {X.tags = X.fromMap newMap}
      | otherwise = seg


-- | Extract marginal annotations from the given sentence.
extract :: Sent w t -> Anno t Double
extract = fmap $ X.unWMap . X.tags


----------------------
-- Disambiguation
----------------------


-- | Find all optimal paths in the given annotation. Optimal paths are those
-- which go through tags with the assigned probability 1. For a given chosen
-- edge, all the tags with probability 1 are selected.
findOptimalPaths :: Ord t => Anno t Double -> [[(EdgeID, S.Set t)]]
findOptimalPaths dag = do
  edgeID <- DAG.dagEdges dag
  guard $ DAG.isInitialEdge edgeID dag
  doit edgeID
  where
    doit i = inside i ++ final i
    inside i = do
      let tags =
            [ tak
            | (tak, weight) <- M.toList (DAG.edgeLabel i dag)
            , weight >= 1.0 - eps ]
      guard . not $ null tags
      j <- DAG.nextEdges i dag
      xs <- doit j
      return $ (i, S.fromList tags) : xs
    final i = do
      guard $ DAG.isFinalEdge i dag
      let tags =
            [ tak
            | (tak, weight) <- M.toList (DAG.edgeLabel i dag)
            , weight >= 1.0 - eps ]
      guard . not $ null tags
      return [(i, S.fromList tags)]
    eps = 1.0e-9


-- | Make the given path with disamb markers in the given annotation
-- and produce a new disamb annotation.
disambPath :: (Ord t) => [(EdgeID, S.Set t)] -> Anno t Double -> Anno t Bool
disambPath path =
  DAG.mapE doit
  where
    pathMap = M.fromList path
    doit edgeID m = M.fromList $ do
      let onPath = maybe S.empty id $ M.lookup edgeID pathMap
      x <- M.keys m
      return (x, S.member x onPath)


-- | Determine max probabilities corresponding to individual tags w.r.t. the
-- disambiguation model.
disamb :: (X.Word w, Ord t) => D.Disamb t -> Sent w t -> Anno t Bool
disamb dmb = D.disamb dmb


----------------------
-- Marginals and Probs
----------------------


-- | Determine marginal probabilities corresponding to individual tags w.r.t.
-- the guessing model.
guessMarginals
  :: (X.Word w, Ord t)
  => CRF.Config P.Tag
  -> G.Guesser t P.Tag
  -> Sent w t
  -> Anno t Double
guessMarginals cfg gsr = fmap X.unWMap . G.marginals cfg gsr


-- | Determine marginal probabilities corresponding to individual tags w.r.t.
-- the disambiguation model.
disambMarginals :: (X.Word w, Ord t) => D.Disamb t -> Sent w t -> Anno t Double
disambMarginals = disambProbs D.Marginals


-- | Determine max probabilities corresponding to individual tags w.r.t. the
-- disambiguation model.
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
      then edge {X.tags = trimWMap k (X.tags edge)}
      else edge
    trimWMap n = X.fromMap . trimMap n . X.unWMap


-- | Trim down the set of potential labels to the `k` most probable ones.
trimMap :: (Ord t) => Int -> M.Map t Double -> M.Map t Double
trimMap k
  = M.fromList
  . take k
  . reverse
  . sortBy (comparing snd)
  . M.toList


---------------------
-- Tagging
---------------------


-- | Extend the OOV words with new, guessed interpretations.
--
-- Determine marginal probabilities corresponding to individual tags w.r.t.
-- the guessing model and, afterwards, trim the sentence to keep only the `k`
-- most probably labels for each OOV edge. Note that, for OOV words, the entire
-- set of default tags is considered.
--
guessSent ::
     (X.Word w, Ord t)
  => Int
  -> CRF.Config P.Tag
  -> G.Guesser t P.Tag
  -> Sent w t
  -> Sent w t
guessSent k cfg gsr sent =
  insertGuessed (fmap (trimMap k) (guessMarginals cfg gsr sent)) sent


-- | Perform guessing, trimming, and finally determine marginal probabilities
-- corresponding to individual tags w.r.t. the guessing model.
guess ::
     (X.Word w, Ord t)
  => Int
  -> CRF.Config P.Tag
  -> G.Guesser t P.Tag
  -> Sent w t
  -> Anno t Double
guess k cfg gsr sent =
  extract . trimOOV k $ replace (guessMarginals cfg gsr sent) sent


-- | Perform guessing, trimming, and finally determine marginal probabilities
-- corresponding to individual tags w.r.t. the disambiguation model.
tag ::
     (X.Word w, Ord t)
  => Int
  -> CRF.Config P.Tag
  -> Concraft t
  -> Sent w t
  -> Anno t Double
tag k cfg crf = disambMarginals (disamber crf) . guessSent k cfg (guesser crf)


---------------------
-- Training
---------------------


-- -- | Train the `Concraft` model.
-- -- No reanalysis of the input data will be performed.
-- --
-- -- The `FromJSON` and `ToJSON` instances are used to store processed
-- -- input data in temporary files on a disk.
-- train
--     :: (X.Word w, Ord t)
--     => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
--                             --   of the training and evaluation input data
--                             --   must correspond.
--     -> Int                  -- ^ How many tags is the guessing model supposed
--                             --   to produce for a given OOV word?  It will be
--                             --   used (see `G.guessSent`) on both training and
--                             --   evaluation input data prior to the training
--                             --   of the disambiguation model.
--     -> G.TrainConf t P.Tag  -- ^ Training configuration for the guessing model.
--     -> D.TrainConf t        -- ^ Training configuration for the
--                             --   disambiguation model.
--     -> IO [Sent w t]    -- ^ Training dataset.  This IO action will be
--                             --   executed a couple of times, so consider using
--                             --   lazy IO if your dataset is big.
--     -> IO [Sent w t]    -- ^ Evaluation dataset IO action.  Consider using
--                             --   lazy IO if your dataset is big.
--     -> IO (Concraft t)
-- train tagset guessNum guessConf disambConf trainR'IO evalR'IO = do
--   Temp.withTempDirectory "." ".guessed" $ \tmpDir -> do
--   let temp = withTemp tagset tmpDir
--
--   putStrLn "\n===== Train guessing model ====="
--   guesser <- G.train guessConf trainR'IO evalR'IO
--   let guess = guessSent guessNum guesser
--   trainG  <- map guess <$> trainR'IO
--   evalG   <- map guess <$> evalR'IO
--
--   temp "train" trainG $ \trainG'IO -> do
--   temp "eval"  evalG  $ \evalG'IO  -> do
--
--   putStrLn "\n===== Train disambiguation model ====="
--   disamb <- D.train disambConf trainG'IO evalG'IO
--   return $ Concraft tagset guessNum guesser disamb
--
--
-- ---------------------
-- -- Temporary storage
-- ---------------------
--
--
-- -- | Store dataset on a disk and run a handler on a list which is read
-- -- lazily from the disk.  A temporary file will be automatically
-- -- deleted after the handler is done.
-- --
-- -- NOTE: (11/11/2017): it's just a dummy function right now, which does
-- -- not use disk storage at all.
-- --
-- withTemp
--   -- :: (FromJSON w, ToJSON w)
--   :: P.Tagset
--   -> FilePath                     -- ^ Directory to create the file in
--   -> String                       -- ^ Template for `Temp.withTempFile`
--   -> [Sent w t]                   -- ^ Input dataset
--   -> (IO [Sent w t] -> IO a)      -- ^ Handler
--   -> IO a
-- withTemp _      _   _    [] handler = handler (return [])
-- withTemp tagset dir tmpl xs handler =
--   Temp.withTempFile dir tmpl $ \tmpPath tmpHandle -> do
--     hClose tmpHandle
--     let txtSent = X.mapSent $ P.showTag tagset
--         tagSent = X.mapSent $ P.parseTag tagset
--     handler (return xs)


---------------------
-- Pruning
---------------------


-- | Prune the disambiguation model: discard model features with absolute values
-- (in log-domain) lower than the given threshold.
prune :: Double -> Concraft t -> Concraft t
prune x concraft =
    let disamber' = D.prune x (disamber concraft)
    in  concraft { disamber = disamber' }
