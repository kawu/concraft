{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft
(
-- * Model 
  Concraft (..)

-- * Tagging
, tag

-- * Training
, train
) where

import           System.IO (hClose)
import           Control.Applicative ((<$>), (<*>))
import           Data.Binary (Binary, put, get)
import           Data.Aeson
import           Data.Maybe (fromJust)
import qualified System.IO.Temp as Temp

import           NLP.Concraft.Morphosyntax
import           NLP.Concraft.Analysis
import           NLP.Concraft.Format.Temp
import qualified Data.Tagset.Positional as P
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D


---------------------
-- Concraft
---------------------


-- | Concraft data.
data Concraft = Concraft
    { tagset        :: P.Tagset
    , guessNum      :: Int
    , guesser       :: G.Guesser P.Tag
    , disamb        :: D.Disamb }

instance Binary Concraft where
    put Concraft{..} = do
        put tagset
        put guessNum
        put guesser
        put disamb
    get = Concraft <$> get <*> get <*> get <*> get


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


-- | Train guessing and disambiguation models.
-- TODO: We take an input dataset as a list, since it is read only once.
-- TODO: Use some legible format to store temporary files.
train
    :: (Word w, FromJSON w, ToJSON w)
    => P.Tagset         -- ^ Tagset
    -> Analyse w P.Tag  -- ^ Analysis function
    -> Int              -- ^ Numer of guessed tags for each word 
    -> G.TrainConf      -- ^ Guessing model training configuration
    -> D.TrainConf      -- ^ Disambiguation model training configuration
    -> [SentO w P.Tag]  -- ^ Training data
    -> Maybe [SentO w P.Tag]  -- ^ Maybe evaluation data
    -> IO Concraft
train tagset ana guessNum guessConf disambConf train0 eval0 = do
    putStrLn "\n===== Reanalysis ====="
    trainR <- reAnaPar tagset ana train0
    evalR  <- case eval0 of
            Just ev -> Just <$> reAnaPar tagset ana ev
            Nothing -> return Nothing
    withTemp tagset "train" trainR $ \trainR'IO -> do
    withTemp' tagset "eval" evalR  $ \evalR'IO  -> do

    putStrLn "\n===== Train guessing model ====="
    guesser <- do
        tr <- trainR'IO
        ev <- evalR'IO
        G.train guessConf tr ev
    trainG <-       map (G.guessSent guessNum guesser)  <$> trainR'IO
    evalG  <- fmap (map (G.guessSent guessNum guesser)) <$> evalR'IO

    putStrLn "\n===== Train disambiguation model ====="
    disamb <- D.train disambConf trainG evalG
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
    -> String                       -- ^ Template for `Temp.withTempFile`
    -> [Sent w P.Tag]               -- ^ Input dataset
    -> (IO [Sent w P.Tag] -> IO a)  -- ^ Handler
    -> IO a
withTemp tagset tmpl xs handler =
    withTemp' tagset tmpl (Just xs) (handler . fmap fromJust)


-- | Similar to `withTemp` but on a `Maybe` dataset.
--
-- Store dataset on a disk and run a handler on a list which is read
-- lazily from the disk.  A temporary file will be automatically
-- deleted after the handler is done.
withTemp'
    :: (FromJSON w, ToJSON w)
    => P.Tagset
    -> String
    -> Maybe [Sent w P.Tag]
    -> (IO (Maybe [Sent w P.Tag]) -> IO a)
    -> IO a
withTemp' tagset tmpl (Just xs) handler =
  Temp.withTempFile "." tmpl $ \tmpPath tmpHandle -> do
    hClose tmpHandle
    let txtSent = mapSent $ P.showTag tagset
        tagSent = mapSent $ P.parseTag tagset
    writePar tmpPath $ map txtSent xs
    handler (Just . map tagSent <$> readPar tmpPath)
withTemp' _ _ Nothing handler = handler (return Nothing)
