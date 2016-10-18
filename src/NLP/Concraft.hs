{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft
(
-- * Model
  Concraft (..)
, saveModel
, loadModel

-- * Tagging
, tag
, marginals

-- * Training
, train
, reAnaTrain

-- * Pruning
, prune
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

import           NLP.Concraft.Morphosyntax
import           NLP.Concraft.Analysis
import           NLP.Concraft.Format.Temp
import qualified Data.Tagset.Positional as P
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D


---------------------
-- Model
---------------------


modelVersion :: String
modelVersion = "0.7"


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


-- | Tag sentence using the model.  In your code you should probably
-- use your analysis function, translate results into a container of
-- `Sent`ences, evaluate `tag` on each sentence and embed the
-- tagging results into the morphosyntactic structure of your own.
--
-- The function returns guessing results as `fst` elements
-- of the output pairs and disambiguation results as `snd`
-- elements of the corresponding pairs.
tag :: Word w => Concraft -> Sent w P.Tag -> [(S.Set P.Tag, P.Tag)]
tag Concraft{..} sent =
    zip (map S.fromList gss) tgs
  where
    gss = G.guess guessNum guesser sent
    tgs = D.disamb disamb (G.include gss sent)


-- | Determine marginal probabilities corresponding to individual
-- tags w.r.t. the disambiguation model.  Since the guessing model
-- is used first, the resulting weighted maps corresponding to OOV
-- words may contain tags not present in the input sentence.
marginals :: Word w => Concraft -> Sent w P.Tag -> [WMap P.Tag]
marginals Concraft{..} sent =
    let gss = G.guess guessNum guesser sent
    in  D.marginals disamb (G.include gss sent)


---------------------
-- Training
---------------------


-- | Train the `Concraft` model after dataset reanalysis.
--
-- The `FromJSON` and `ToJSON` instances are used to store processed
-- input data in temporary files on a disk.
reAnaTrain
    :: (Word w, FromJSON w, ToJSON w)
    => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
                            --   of the training and evaluation input data
                            --   must correspond.
    -> Analyse w P.Tag      -- ^ Analysis function.  It will be used to
                            --   reanalyse input dataset.
    -> Int                  -- ^ How many tags is the guessing model supposed
                            --   to produce for a given OOV word?  It will be 
                            --   used (see `G.guessSent`) on both training and
                            --   evaluation input data prior to the training
                            --   of the disambiguation model.
    -> G.TrainConf          -- ^ Training configuration for the guessing model.
    -> D.TrainConf          -- ^ Training configuration for the
                            --   disambiguation model.
    -> IO [SentO w P.Tag]   -- ^ Training dataset.  This IO action will be
                            --   executed a couple of times, so consider using
                            --   lazy IO if your dataset is big. 
    -> IO [SentO w P.Tag]   -- ^ Evaluation dataset IO action.  Consider using
                            --   lazy IO if your dataset is big.
    -> IO Concraft
reAnaTrain tagset ana guessNum guessConf disambConf train0'IO eval0'IO = do
    Temp.withTempDirectory "." ".reana" $ \tmpDir -> do
    let temp = withTemp tagset tmpDir

    putStrLn "\n===== Reanalysis ====="
    trainR <- reAnaPar tagset ana =<< train0'IO
    evalR  <- reAnaPar tagset ana =<< eval0'IO

    temp "train" trainR $ \trainR'IO -> do
    temp "eval"  evalR  $ \evalR'IO  -> do
    train tagset guessNum guessConf disambConf trainR'IO evalR'IO


-- | Train the `Concraft` model.
-- No reanalysis of the input data will be performed.
--
-- The `FromJSON` and `ToJSON` instances are used to store processed
-- input data in temporary files on a disk.
train
    :: (Word w, FromJSON w, ToJSON w)
    => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
                            --   of the training and evaluation input data
                            --   must correspond.
    -> Int                  -- ^ How many tags is the guessing model supposed
                            --   to produce for a given OOV word?  It will be 
                            --   used (see `G.guessSent`) on both training and
                            --   evaluation input data prior to the training
                            --   of the disambiguation model.
    -> G.TrainConf          -- ^ Training configuration for the guessing model.
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
    trainG  <- map (G.guessSent guessNum guesser) <$> trainR'IO
    evalG   <- map (G.guessSent guessNum guesser) <$> evalR'IO
    temp "train" trainG $ \trainG'IO -> do
    temp "eval"  evalG  $ \evalG'IO  -> do

    putStrLn "\n===== Train disambiguation model ====="
    disamb <- D.train disambConf trainG'IO evalG'IO
    return $ Concraft tagset guessNum guesser disamb


---------------------
-- Pruning
---------------------


-- | Prune disambiguation model: discard model features with
-- absolute values (in log-domain) lower than the given threshold.
prune :: Double -> Concraft -> Concraft
prune x concraft =
    let disamb' = D.prune x (disamb concraft)
    in  concraft { disamb = disamb' }


---------------------
-- Temporary storage
---------------------


-- | Store dataset on a disk and run a handler on a list which is read
-- lazily from the disk.  A temporary file will be automatically
-- deleted after the handler is done.
withTemp
    :: (FromJSON w, ToJSON w)
    => P.Tagset
    -> FilePath                     -- ^ Directory to create the file in
    -> String                       -- ^ Template for `Temp.withTempFile`
    -> [Sent w P.Tag]               -- ^ Input dataset
    -> (IO [Sent w P.Tag] -> IO a)  -- ^ Handler
    -> IO a
withTemp _      _   _    [] handler = handler (return [])
withTemp tagset dir tmpl xs handler =
  Temp.withTempFile dir tmpl $ \tmpPath tmpHandle -> do
    hClose tmpHandle
    let txtSent = mapSent $ P.showTag tagset
        tagSent = mapSent $ P.parseTag tagset
    -- writePar tmpPath $ map txtSent xs
    -- handler (map tagSent <$> readPar tmpPath)
    handler (return xs)

-- withTemp
--     :: (FromJSON w, ToJSON w)
--     => P.Tagset
--     -> FilePath                     -- ^ Directory to create the file in
--     -> String                       -- ^ Template for `Temp.withTempFile`
--     -> [Sent w P.Tag]               -- ^ Input dataset
--     -> (IO [Sent w P.Tag] -> IO a)  -- ^ Handler
--     -> IO a
-- withTemp _      _   _    [] handler = handler (return [])
-- withTemp tagset dir tmpl xs handler =
--   Temp.withTempFile dir tmpl $ \tmpPath tmpHandle -> do
--     hClose tmpHandle
--     let txtSent = mapSent $ P.showTag tagset
--         tagSent = mapSent $ P.parseTag tagset
--     writePar tmpPath $ map txtSent xs
--     handler (map tagSent <$> readPar tmpPath)
