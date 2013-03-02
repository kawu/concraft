{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft
(
-- * Types
  Concraft (..)
, Analyse

-- * Tagging
, tag
-- , tagSent
-- , tagDoc

-- * Training
-- , train
) where

import System.IO (hClose)
import Control.Applicative ((<$>), (<*>))
import Data.Traversable (Traversable)
import Data.Char (isSpace)
import Data.Binary (Binary, put, get)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified System.IO.Temp as Temp

import qualified Data.Tagset.Positional as P
import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D

-- | Concraft data.
data Concraft a = Concraft
    { tagset        :: P.Tagset
    , analyser      :: a
    , guessNum      :: Int
    , guesser       :: G.Guesser P.Tag
    , disamb        :: D.Disamb }

instance Binary a => Binary (Concraft a) where
    put Concraft{..} = do
        put tagset
        put analyser
        put guessNum
        put guesser
        put disamb
    get = Concraft <$> get <*> get <*> get <*> get <*> get

-- | An analyser performs segmentation (paragraph-, sentence- and token-level)
-- and morphological analysis.  It is represented with a class because we need
-- to be able to serialize the information about the analysis tool in the
-- Concraft model.
class Analyse a where
    analyse :: a -> L.Text -> [X.Sent P.Tag]

-- | Perform morphlogical tagging on the input text.
tag :: Analyse a => Concraft a -> L.Text -> [X.Sent P.Tag]
tag Concraft{..} =
    let doTag = D.include (D.disamb disamb)
              . G.include (G.guess guessNum guesser)
    in  map doTag . analyse analyser

-- | A dataset element.  Original sentence is needed to perform
-- reanalysis of the data.
data Elem = Elem
    { sent  :: X.Sent P.Tag
    , orig  :: T.Text }

-- -- | Train guessing and disambiguation models.
-- train
--     :: Int                  -- ^ Numer of guessed tags for each word 
--     -> G.TrainConf          -- ^ Guessing model training configuration
--     -> D.TrainConf          -- ^ Disambiguation model training configuration
--     -> FilePath             -- ^ Training file
--     -> IO [Elem]            -- ^ Training data (lazy) IO action
--     -> Maybe (IO [Elem])    -- ^ Maybe evaluation data (lazy) IO action
--     -> IO Concraft          -- ^ Resultant models
-- train format guessNum guessConf disambConf trainIO evalIO'Maybe = do
--     putStrLn "\n===== Reanalysis ====\n"
-- 
--     putStrLn "\n===== Train guessing model ====\n"
--     guesser <- G.train format guessConf trainPath evalPath'Maybe
--     let withGuesser = guessFile format guessNum guesser
--     withGuesser "train" (Just trainPath) $ \(Just trainPathG) ->
--       withGuesser "eval"   evalPath'Maybe  $ \evalPathG'Maybe  -> do
--         putStrLn "\n===== Train disambiguation model ====\n"
--         disamb <- D.train format disambConf trainPathG evalPathG'Maybe
--         return $ Concraft guessNum guesser disamb
-- 
-- guessFile
--     :: Functor d
--     => F.Doc d s w              -- ^ Document format handler
--     -> Int                      -- ^ Numer of guessed tags for each word
--     -> G.Guesser P.Tag          -- ^ Guesser
--     -> String                   -- ^ Template for temporary file name
--     -> Maybe FilePath           -- ^ File to guess
--     -> (Maybe FilePath -> IO a) -- ^ Handler
--     -> IO a
-- guessFile _ _ _ _ Nothing handler = handler Nothing
-- guessFile format guessNum gsr tmpl (Just path) handler =
--     Temp.withTempFile "." tmpl $ \tmpPath tmpHandle -> do
--         inp <- L.readFile path
--         let out = G.guessDoc format guessNum gsr inp
--         hClose tmpHandle
--         L.writeFile tmpPath out
--         handler (Just tmpPath)

-- -- | Reanalyse the dataset.
-- reanalyse :: Analyse a => a -> P.Tagset -> [Elem] -> [X.Sent P.Tag]
-- reanalyse tagset elems = chunk
--     (map length reana)
--     (sync tagset
--         (concat gold)
--         (concat reana))
--   where
--     gold    = map sent elems
--     reana   = analyse ana $ L.fromChunks $ map orig elems
-- 
-- -- | Divide the list into a list of chunks given the list of
-- -- lengths of individual chunks.
-- chunk :: [Int] -> [a] -> [[a]]
-- chunk (n:ns) xs = 
--     let (first, rest) = splitAt n xs 
--     in  first : chunk ns rest
-- chunk [] [] = []
