{-# LANGUAGE RecordWildCards #-}


-- | Top-level module adated to DAGs, guessing and disambiguation.


module NLP.Concraft.DAG2
(
-- * Model
  Concraft (..)
, saveModel
, loadModel


-- * Annotation
, Anno
, replace

-- * Marginals
, D.ProbType (..)
, guessMarginals
, disambMarginals
, disambProbs

-- * Tagging
-- , guessSent
, guess
, tag
, tag'

-- * Training
, train

-- * Pruning
, prune
) where


import           System.IO (hClose)
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Control.Monad (when)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Binary (Binary, put, get)
import qualified Data.Binary as Binary
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
import qualified NLP.Concraft.DAG.Disamb as D


---------------------
-- Model
---------------------


modelVersion :: String
modelVersion = "dag2:0.7"


-- | Concraft data.
data Concraft = Concraft
  { tagset        :: P.Tagset
  , guessNum      :: Int
  , guesser       :: G.Guesser P.Tag
  , disamb        :: D.Disamb }


instance Binary Concraft where
    put Concraft{..} = do
        put modelVersion
        put tagset
        put guessNum
        put guesser
        put disamb
    get = do
        comp <- get
        when (comp /= modelVersion) $ error $
            "Incompatible model version: " ++ comp ++
            ", expected: " ++ modelVersion
        Concraft <$> get <*> get <*> get <*> get


-- | Save model in a file.  Data is compressed using the gzip format.
saveModel :: FilePath -> Concraft -> IO ()
saveModel path = BL.writeFile path . GZip.compress . Binary.encode


-- | Load model from a file.
loadModel :: FilePath -> IO Concraft
loadModel path = do
    x <- Binary.decode . GZip.decompress <$> BL.readFile path
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
-- Marginals and Probs
----------------------


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the guessing model.
guessMarginals :: X.Word w => G.Guesser P.Tag -> Sent w P.Tag -> Anno P.Tag Double
guessMarginals gsr = fmap X.unWMap . G.marginals gsr


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the guessing model.
disambMarginals :: X.Word w => D.Disamb -> Sent w P.Tag -> Anno P.Tag Double
disambMarginals dmb = fmap X.unWMap . D.marginals dmb


-- | Determine probabilities corresponding to individual
-- tags w.r.t. the guessing model.
disambProbs :: X.Word w => D.ProbType -> D.Disamb -> Sent w P.Tag -> Anno P.Tag Double
disambProbs typ dmb = fmap X.unWMap . D.probs typ dmb


-------------------------------------------------
-- Trimming
-------------------------------------------------


-- | Trim down the set of potential labels to `k` most probable ones
-- for each OOV word in the sentence.
trimOOV :: X.Word w => Int -> Sent w P.Tag -> Sent w P.Tag
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
-- most probably labels for each edge. Note that, for unknown words, the entire
-- set of default tags is considered.
guessSent :: X.Word w => Int -> G.Guesser P.Tag -> Sent w P.Tag -> Sent w P.Tag
guessSent k gsr sent = trimOOV k $ replace (guessMarginals gsr sent) sent


-- | Perform guessing, trimming, and finally determine marginal probabilities
-- corresponding to individual tags w.r.t. the guessing model.
guess :: X.Word w => Int -> G.Guesser P.Tag -> Sent w P.Tag -> Anno P.Tag Double
guess k gsr = extract . guessSent k gsr


-- | Perform guessing, trimming, and finally determine marginal probabilities
-- corresponding to individual tags w.r.t. the disambiguation model.
tag :: X.Word w => Int -> Concraft -> Sent w P.Tag -> Anno P.Tag Double
tag k crf = disambMarginals (disamb crf) . guessSent k (guesser crf)


-- | Perform guessing, trimming, and finally determine probabilities
-- corresponding to individual tags w.r.t. the disambiguation model.
tag' :: X.Word w => Int -> D.ProbType -> Concraft -> Sent w P.Tag -> Anno P.Tag Double
tag' k typ Concraft{..} = disambProbs typ disamb . guessSent k guesser


---------------------
-- Training
---------------------


-- | Train the `Concraft` model.
-- No reanalysis of the input data will be performed.
--
-- The `FromJSON` and `ToJSON` instances are used to store processed
-- input data in temporary files on a disk.
train
    :: (X.Word w)
    => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
                            --   of the training and evaluation input data
                            --   must correspond.
    -> Int                  -- ^ How many tags is the guessing model supposed
                            --   to produce for a given OOV word?  It will be
                            --   used (see `G.guessSent`) on both training and
                            --   evaluation input data prior to the training
                            --   of the disambiguation model.
    -> G.TrainConf P.Tag    -- ^ Training configuration for the guessing model.
    -> D.TrainConf          -- ^ Training configuration for the
                            --   disambiguation model.
    -> IO [Sent w P.Tag]    -- ^ Training dataset.  This IO action will be
                            --   executed a couple of times, so consider using
                            --   lazy IO if your dataset is big.
    -> IO [Sent w P.Tag]    -- ^ Evaluation dataset IO action.  Consider using
                            --   lazy IO if your dataset is big.
    -> IO Concraft
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
--   let trainG'IO = trainR'IO
--       evalG'IO = evalR'IO

  putStrLn "\n===== Train disambiguation model ====="
  disamb <- D.train disambConf trainG'IO evalG'IO
  return $ Concraft tagset guessNum guesser disamb


---------------------
-- Temporary storage
---------------------


-- | Store dataset on a disk and run a handler on a list which is read
-- lazily from the disk.  A temporary file will be automatically
-- deleted after the handler is done.
withTemp
  -- :: (FromJSON w, ToJSON w)
  :: P.Tagset
  -> FilePath                     -- ^ Directory to create the file in
  -> String                       -- ^ Template for `Temp.withTempFile`
  -> [Sent w P.Tag]               -- ^ Input dataset
  -> (IO [Sent w P.Tag] -> IO a)  -- ^ Handler
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
prune :: Double -> Concraft -> Concraft
prune x concraft =
    let disamb' = D.prune x (disamb concraft)
    in  concraft { disamb = disamb' }

