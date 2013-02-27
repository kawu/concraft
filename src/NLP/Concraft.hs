{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft
(
-- * Types
  Concraft (..)

-- * Tagging
, tag
, tagSent
, tagDoc

-- * Training
, train
) where

import System.IO (hClose)
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable)
import Data.Binary (Binary, put, get)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified System.IO.Temp as Temp

import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D

-- | Concraft data.
data Concraft = Concraft
    { guessNum      :: Int
    , guesser       :: G.Guesser F.Tag
    , disamb        :: D.Disamb }

instance Binary Concraft where
    put Concraft{..} = do
        put guessNum
        put guesser
        put disamb
    get = Concraft <$> get <*> get <*> get

-- | Perform disambiguation preceded by context-sensitive guessing.
tag :: Concraft -> Mx.Sent F.Tag -> [F.Tag]
tag Concraft{..} sent
    = D.disamb disamb
    . G.include sent 
    . G.guess guessNum guesser 
    $ sent

-- | Tag the sentence.
tagSent :: F.Sent s w -> Concraft -> s -> s
tagSent sentH Concraft{..}
    = D.disambSent sentH disamb
    . G.guessSent  sentH guessNum guesser

-- | Tag document.
tagDoc :: Functor f => F.Doc f s w -> Concraft -> L.Text -> L.Text
tagDoc F.Doc{..} concraft =
    let onSent = tagSent sentHandler concraft
    in  showDoc . fmap onSent . parseDoc

-- | Train guessing and disambiguation models.
train
    :: (Functor f, Foldable f)
    => F.Doc f s w      -- ^ Document format handler
    -> Int              -- ^ Numer of guessed tags for each word 
    -> G.TrainConf      -- ^ Guessing model training configuration
    -> D.TrainConf      -- ^ Disambiguation model training configuration
    -> FilePath         -- ^ Training file
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO Concraft      -- ^ Resultant models
train format guessNum guessConf disambConf trainPath evalPath'Maybe = do
    putStrLn "\n===== Train guessing model ====\n"
    guesser <- G.train format guessConf trainPath evalPath'Maybe
    let withGuesser = guessFile format guessNum guesser
    withGuesser "train" (Just trainPath) $ \(Just trainPathG) ->
      withGuesser "eval"   evalPath'Maybe  $ \evalPathG'Maybe  -> do
        putStrLn "\n===== Train disambiguation model ====\n"
        disamb <- D.train format disambConf trainPathG evalPathG'Maybe
        return $ Concraft guessNum guesser disamb

guessFile
    :: Functor f
    => F.Doc f s w              -- ^ Document format handler
    -> Int                      -- ^ Numer of guessed tags for each word
    -> G.Guesser F.Tag          -- ^ Guesser
    -> String                   -- ^ Template for temporary file name
    -> Maybe FilePath           -- ^ File to guess
    -> (Maybe FilePath -> IO a) -- ^ Handler
    -> IO a
guessFile _ _ _ _ Nothing handler = handler Nothing
guessFile format guessNum gsr tmpl (Just path) handler =
    Temp.withTempFile "." tmpl $ \tmpPath tmpHandle -> do
        inp <- L.readFile path
        let out = G.guessDoc format guessNum gsr inp
        hClose tmpHandle
        L.writeFile tmpPath out
        handler (Just tmpPath)
