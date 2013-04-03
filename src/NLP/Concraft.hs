{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft
(
-- * Types
  Concraft (..)
, Analyse

-- * Tagging
, tag
, tagSent
, tagPar

-- * Training
, train
, trainNoAna
) where

import System.IO (hClose)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.Binary (Binary, put, get, encodeFile, decodeFile)
import qualified Data.Text.Lazy as L
import qualified System.IO.Temp as Temp

import qualified Data.Tagset.Positional as P
import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D

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

-- | An analyser performs segmentation (paragraph-, sentence- and token-level)
-- and morphological analysis.
type Analyse = L.Text -> [[X.Sent P.Tag]]

-- | Perform morphlogical tagging on the input text.
tag :: Analyse -> Concraft -> L.Text -> [[X.Sent P.Tag]]
tag analyse concraft = map (tagPar concraft) . analyse

-- Tag sentence.  The function doesn't perform reanalysis.
tagSent :: Concraft -> X.Sent P.Tag -> X.Sent P.Tag
tagSent Concraft{..} =
    D.disambSent disamb . G.guessSent guessNum guesser

-- Tag paragraph.  The function doesn't perform reanalysis.
tagPar :: Concraft -> [X.Sent P.Tag] -> [X.Sent P.Tag]
tagPar = map . tagSent

-- | Train guessing and disambiguation models.
trainNoAna
    :: P.Tagset         -- ^ Tagset
    -> Int              -- ^ Numer of guessed tags for each word 
    -> G.TrainConf      -- ^ Guessing model training configuration
    -> D.TrainConf      -- ^ Disambiguation model training configuration
    -> [X.Sent P.Tag]   -- ^ Training data
    -> Maybe [X.Sent P.Tag] -- ^ Maybe evaluation data
    -> IO Concraft
trainNoAna tagset guessNum guessConf disambConf trainR evalR = do
    withTemp "train" trainR $ \trainR'IO -> do
    withTemp' "eval" evalR  $ \evalR'IO  -> do

    putStrLn "\n===== Train guessing model ====\n"
    guesser <- do
        tr <- trainR'IO
        ev <- evalR'IO
        G.train guessConf tr ev
    trainG <-       map (G.guessSent guessNum guesser)  <$> trainR'IO
    evalG  <- fmap (map (G.guessSent guessNum guesser)) <$> evalR'IO

    putStrLn "\n===== Train disambiguation model ====\n"
    disamb <- D.train disambConf trainG evalG
    return $ Concraft tagset guessNum guesser disamb

-- | Train guessing and disambiguation models.
-- TODO: Its probably a good idea to take an input dataset as a list, since
-- it is read only once.  On the other hand, we loose a chance to handle
-- errors generated during dataset parsing.  So, perhaps pipes would be
-- better?  Besides, even when a user uses plain lists as a representation
-- of datasets, they can be easily transormed into pipes.
-- TODO: Use some legible format to store temporary files, for users sake.
train
    :: P.Tagset         -- ^ Tagset
    -> Analyse          -- ^ Analysis function
    -> Int              -- ^ Numer of guessed tags for each word 
    -> G.TrainConf      -- ^ Guessing model training configuration
    -> D.TrainConf      -- ^ Disambiguation model training configuration
    -> [X.Sent P.Tag]   -- ^ Training data
    -> Maybe [X.Sent P.Tag]    -- ^ Maybe evaluation data
    -> IO Concraft
train tagset ana guessNum guessConf disambConf train0 eval0 = do
    putStrLn "\n===== Reanalysis ====\n"
    let trainR = reanalyse tagset ana train0
        evalR  = case eval0 of
            Just ev -> Just $ reanalyse tagset ana ev
            Nothing -> Nothing
    withTemp "train" trainR $ \trainR'IO -> do
    withTemp' "eval" evalR  $ \evalR'IO  -> do

    putStrLn "\n===== Train guessing model ====\n"
    guesser <- do
        tr <- trainR'IO
        ev <- evalR'IO
        G.train guessConf tr ev
    trainG <-       map (G.guessSent guessNum guesser)  <$> trainR'IO
    evalG  <- fmap (map (G.guessSent guessNum guesser)) <$> evalR'IO

    putStrLn "\n===== Train disambiguation model ====\n"
    disamb <- D.train disambConf trainG evalG
    return $ Concraft tagset guessNum guesser disamb

-- | Store dataset on a disk and run a handler on a lazy list which is read
-- directly from the disk.  A temporary file will be automatically
-- deleted after the handler is done.
-- TODO: Try using pipes instead of lazy IO (input being a pipe as well?).
withTemp
    :: String                       -- ^ Template for `Temp.withTempFile`
    -> [X.Sent P.Tag]               -- ^ Input dataset
    -> (IO [X.Sent P.Tag] -> IO a)  -- ^ Handler
    -> IO a
withTemp tmpl xs handler = withTemp' tmpl (Just xs) (handler . fmap fromJust)

-- | The same as `withTemp` but on a `Maybe` dataset.
withTemp' :: String
          -> Maybe [X.Sent P.Tag]
          -> (IO (Maybe [X.Sent P.Tag]) -> IO a)
          -> IO a
withTemp' tmpl (Just xs) handler =
  Temp.withTempFile "." tmpl $ \tmpPath tmpHandle -> do
    hClose tmpHandle
    encodeFile tmpPath xs
    handler (decodeFile tmpPath)
withTemp' _ Nothing handler = handler (return Nothing)

-- | Reanalyse paragraph.
reanalyse :: P.Tagset -> Analyse -> [X.Sent P.Tag] -> [X.Sent P.Tag]
reanalyse tagset ana gold = chunk
    (map length reana)
    (X.sync tagset (concat gold) (concat reana))
  where
    reana = concat . ana . L.fromChunks . map X.restore $ gold

-- | Divide the list into a list of chunks given the list of
-- lengths of individual chunks.
chunk :: [Int] -> [a] -> [[a]]
chunk (n:ns) xs = 
    let (first, rest) = splitAt n xs 
    in  first : chunk ns rest
chunk [] [] = []
chunk [] _  = error "chunk: absurd"
