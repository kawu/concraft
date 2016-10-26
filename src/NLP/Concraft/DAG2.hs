{-# LANGUAGE RecordWildCards #-}


-- | Top-level module adated to DAGs, guessing and disambiguation.


module NLP.Concraft.DAG2
(
-- * Model
  Concraft (..)
, saveModel
, loadModel

-- * Tagging
, marginalsSent
, marginals
-- , tag
-- , marginals

-- * Training
, train

-- -- * Pruning
-- , prune
) where


import           System.IO (hClose)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)
import qualified Data.Set as S
import           Data.Binary (Binary, put, get)
import qualified Data.Binary as Binary
import           Data.Aeson
import qualified System.IO.Temp as Temp
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

import           Data.DAG (DAG)

import qualified Data.Tagset.Positional as P

import           NLP.Concraft.Analysis
import           NLP.Concraft.Format.Temp
import           NLP.Concraft.DAG.Morphosyntax
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


---------------------
-- Tagging
---------------------


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the guessing model.
marginalsSent :: Word w => Concraft -> Sent w P.Tag -> Sent w P.Tag
marginalsSent Concraft{..} = G.marginalsSent guesser


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the guessing model.
marginals :: Word w => Concraft -> Sent w P.Tag -> DAG () (WMap P.Tag)
marginals Concraft{..} = G.marginals guesser


---------------------
-- Training
---------------------


-- | Train the `Concraft` model.
-- No reanalysis of the input data will be performed.
--
-- The `FromJSON` and `ToJSON` instances are used to store processed
-- input data in temporary files on a disk.
train
    :: (Word w)
    => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
                            --   of the training and evaluation input data
                            --   must correspond.
    -> Int                  -- ^ How many tags is the guessing model supposed
                            --   to produce for a given OOV word?  It will be
                            --   used (see `G.guessSent`) on both training and
                            --   evaluation input data prior to the training
                            --   of the disambiguation model.
    -> G.TrainConf          -- ^ Training configuration for the guessing model.
    -> IO [Sent w P.Tag]    -- ^ Training dataset.  This IO action will be
                            --   executed a couple of times, so consider using
                            --   lazy IO if your dataset is big.
    -> IO [Sent w P.Tag]    -- ^ Evaluation dataset IO action.  Consider using
                            --   lazy IO if your dataset is big.
    -> IO Concraft
train tagset guessNum guessConf trainR'IO evalR'IO = do
    Temp.withTempDirectory "." ".guessed" $ \tmpDir -> do
    putStrLn "\n===== Train guessing model ====="
    guesser <- G.train guessConf trainR'IO evalR'IO
    return $ Concraft tagset guessNum guesser
