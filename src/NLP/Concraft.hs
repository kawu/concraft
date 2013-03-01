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
import Data.Binary (Binary, put, get)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified System.IO.Temp as Temp

import qualified Data.Tagset.Positional as T
import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D

-- | Concraft data.
data Concraft a = Concraft
    { tagset        :: T.Tagset
    , analyser      :: a
    , guessNum      :: Int
    , guesser       :: G.Guesser T.Tag
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
    analyse :: a -> L.Text -> [X.Sent T.Tag]

-- | Perform morphlogical tagging on the input text.
tag :: Analyse a => Concraft a -> L.Text -> [X.Sent T.Tag]
tag Concraft{..} =
    let doTag = D.include (D.disamb disamb)
              . G.include (G.guess guessNum guesser)
    in  map doTag . analyse analyser

-- -- | Train guessing and disambiguation models.
-- train
--     :: Traversable d
--     => F.Doc d s w      -- ^ Document format handler
--     -> Int              -- ^ Numer of guessed tags for each word 
--     -> G.TrainConf      -- ^ Guessing model training configuration
--     -> D.TrainConf      -- ^ Disambiguation model training configuration
--     -> FilePath         -- ^ Training file
--     -> Maybe FilePath   -- ^ Maybe eval file
--     -> IO Concraft      -- ^ Resultant models
-- train format guessNum guessConf disambConf trainPath evalPath'Maybe = do
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
--     -> G.Guesser T.Tag          -- ^ Guesser
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
