{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft
(
-- * Model 
  Concraft (..)
, saveModel
, loadModel

-- * Tagging
, tag

-- * Training
, train
, reAnaTrain

-- * Pruning
, prune
) where


import           System.IO (hClose)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)
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
-- tagging results into morphosyntactic structure of your own.
--
-- The function returns guessing results as `fst` elements
-- of the output pairs and disambiguation results as `snd`
-- elements of the corresponding pairs.
tag :: Word w => Concraft -> Sent w P.Tag -> [([P.Tag], P.Tag)]
tag Concraft{..} sent =
    zip gss tgs
  where
    gss = G.guess guessNum guesser sent
    tgs = D.disamb disamb (G.include gss sent)


-- tag :: Word w => Concraft -> Sent w P.Tag -> [P.Tag]
-- tag Concraft{..} = D.disamb disamb . G.guessSent guessNum guesser


---------------------
-- Training
---------------------


-- | Train guessing and disambiguation models after dataset reanalysis.
reAnaTrain
    :: (Word w, FromJSON w, ToJSON w)
    => P.Tagset             -- ^ Tagset
    -> Analyse w P.Tag      -- ^ Analysis function
    -> Int                  -- ^ Numer of guessed tags for each word 
    -> G.TrainConf          -- ^ Guessing model training configuration
    -> D.TrainConf          -- ^ Disambiguation model training configuration
    -> IO [SentO w P.Tag]   -- ^ Training data
    -> IO [SentO w P.Tag]   -- ^ Evaluation data
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


-- | Train guessing and disambiguation models.
-- No reanalysis will be performed.
train
    :: (Word w, FromJSON w, ToJSON w)
    => P.Tagset             -- ^ Tagset
    -> Int                  -- ^ Numer of guessed tags for each word
    -> G.TrainConf          -- ^ Guessing model training configuration
    -> D.TrainConf          -- ^ Disambiguation model training configuration
    -> IO [Sent w P.Tag]    -- ^ Training data
    -> IO [Sent w P.Tag]    -- ^ Evaluation data
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


-- | Prune disambiguation model: discard model features with absolute values
-- (in log-domain) lower than the given threshold.
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
    writePar tmpPath $ map txtSent xs
    handler (map tagSent <$> readPar tmpPath)
