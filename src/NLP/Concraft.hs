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
modelVersion = "0.5"


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
-- `Sent`ences, evaluate `tagSent` on each sentence and embed the
-- tagging results into morphosyntactic structure of your own.
tag :: Word w => Concraft -> Sent w P.Tag -> [P.Tag]
tag Concraft{..} = D.disamb disamb . G.guessSent guessNum guesser


---------------------
-- Training
---------------------


-- INFO: We take an input dataset as a list, since it is read only once.

-- | Train guessing and disambiguation models.
train
    :: (Word w, FromJSON w, ToJSON w)
    => P.Tagset         -- ^ Tagset
    -> Analyse w P.Tag  -- ^ Analysis function
    -> Int              -- ^ Numer of guessed tags for each word 
    -> G.TrainConf      -- ^ Guessing model training configuration
    -> D.TrainConf      -- ^ Disambiguation model training configuration
    -> [SentO w P.Tag]  -- ^ Training data
    -> [SentO w P.Tag]  -- ^ Evaluation data
    -> IO Concraft
train tagset ana guessNum guessConf disambConf train0 eval0 = do
    Temp.withTempDirectory "." ".tmp" $ \tmpDir -> do
    let temp = withTemp tagset tmpDir

    putStrLn "\n===== Reanalysis ====="
    trainR <- reAnaPar tagset ana train0
    evalR  <- reAnaPar tagset ana eval0
    temp "train-reana" trainR $ \trainR'IO -> do
    temp "eval-reana"  evalR  $ \evalR'IO  -> do

    putStrLn "\n===== Train guessing model ====="
    guesser <- G.train guessConf trainR'IO evalR'IO
    trainG  <- map (G.guessSent guessNum guesser) <$> trainR'IO
    evalG   <- map (G.guessSent guessNum guesser) <$> evalR'IO
    temp "train-guessed" trainG $ \trainG'IO -> do
    temp "eval-guessed"  evalG  $ \evalG'IO  -> do

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
