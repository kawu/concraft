{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft
( GuessConf (..)
, GuessData (..)
, DisambConf (..)
, DisambWith (..)
, DisambTag
, DisambTrain
, disamb
, disambDoc
, trainOn
) where

-- import Data.Binary (Binary, put, get)
-- import qualified Data.Text as T

import System.IO (hClose)
import Data.Foldable (Foldable)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Numeric.SGD as SGD
import qualified System.IO.Temp as Temp

import NLP.Concraft.Schema
import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D

-- | Guessing configuration.
data GuessConf r = GuessConf
    { guessNum      :: Int
    , guessSchema   :: Schema r () }

-- | Guessing configuration and model data.
data GuessData r = GuessData
    { guessConf     :: GuessConf r
    , guesser       :: G.Guesser r }

-- | Disambiguation configuration.
data DisambConf r t = DisambConf
    { split         :: D.Split r t
    , disambSchema  :: Schema t () }

-- | Disambiguation configuration with...
data DisambWith r t a = DisambWith
    { disambConf    :: DisambConf r t
    , disambWith    :: a }

-- | Tagging with disambiguation configuration.
type DisambTag r t = DisambWith r t (D.TagCRF Ob t)

-- | Training disambiguation model configuration.
type DisambTrain r t c = DisambWith r t (D.TrainCRF Ob t c)

-- | Perform disambiguation preceded by context-sensitive guessing.
disamb
    :: (Ord r, Ord t)
    => GuessData r      -- ^ Guessing configuration
    -> DisambTag r t    -- ^ Disambiguation configuration
    -> Mx.Sent r        -- ^ Input
    -> [r]              -- ^ Output
disamb GuessData{..} DisambWith{..} sent
    = D.disamb disambSchema split tagCRF
    . G.include sent 
    . G.guess guessNum guessSchema guesser 
    $ sent
  where
    GuessConf{..}   = guessConf
    DisambConf{..}  = disambConf
    tagCRF          = disambWith

-- | Tag the sentence.
disambSent
    :: Ord t
    => F.Sent s w
    -> GuessData F.Tag
    -> DisambTag F.Tag t
    -> s -> s
disambSent F.Sent{..} guessData disambTag sent =
  flip mergeSent sent
    [ select wMap orig
    | (wMap, orig) <- zip
        (doDmb sent)
        (parseSent sent) ]
  where
    F.Word{..} = wordHandler
    doDmb orig =
        let xs = map extract (parseSent orig)
        in  map (uncurry mkChoice)
                (zip xs (disamb guessData disambTag xs))
    mkChoice word x = Mx.mkWMap
        [ if x == y
            then (x, 1)
            else (x, 0)
        | y <- Mx.interps word ]

-- | Tag document.
disambDoc
    :: (Functor f, Ord t)
    => F.Doc f s w          -- ^ Document format handler
    -> GuessData F.Tag      -- ^ Guessing configuration
    -> DisambTag F.Tag t    -- ^ Disambiguation configuration
    -> L.Text               -- ^ Input
    -> L.Text               -- ^ Output
disambDoc F.Doc{..} guessData disambTag  =
    let onSent = disambSent sentHandler guessData disambTag
    in  showDoc . fmap onSent . parseDoc

-- | Train guessing and disambiguation models.
trainOn
    :: (Functor f, Foldable f, Ord t)
    => F.Doc f s w              -- ^ Document format handler
    -> GuessConf F.Tag          -- ^ Guessing configuration
    -> SGD.SgdArgs              -- ^ SGD params for guesser
    -> DisambTrain F.Tag t c    -- ^ Disambiguation configuration
    -> FilePath                 -- ^ Training file
    -> Maybe FilePath           -- ^ Maybe eval file
    -> IO (G.Guesser F.Tag, c)  -- ^ Resultant models
trainOn format guessConf@GuessConf{..} sgdArgs DisambWith{..}
        trainPath evalPath'Maybe = do
    putStrLn "\n===== Train guessing model ====\n"
    guesser <- G.trainOn format guessSchema sgdArgs
                    trainPath evalPath'Maybe
    let guessData = GuessData guessConf guesser
    let withGuesser = guessFile format guessData
    withGuesser "train" (Just trainPath) $ \(Just trainPathG) ->
      withGuesser "eval"   evalPath'Maybe  $ \evalPathG'Maybe  -> do
        putStrLn "\n===== Train disambiguation model ====\n"
        let DisambConf{..} = disambConf
        let trainCRF = disambWith
        disambCRF <- D.trainOn format disambSchema split trainCRF
                        trainPathG evalPathG'Maybe
        return (guesser, disambCRF)

guessFile
    :: Functor f
    => F.Doc f s w              -- ^ Document format handler
    -> GuessData F.Tag          -- ^ Guesser
    -> String                   -- ^ Template for temporary file name
    -> Maybe FilePath           -- ^ File to guess
    -> (Maybe FilePath -> IO a) -- ^ Handler
    -> IO a
guessFile _ _ _ Nothing handler = handler Nothing
guessFile format GuessData{..} tmpl (Just path) handler =
    Temp.withTempFile "." tmpl $ \tmpPath tmpHandle -> do
        inp <- L.readFile path
        let GuessConf{..} = guessConf
        let out = G.guessDoc format guessNum guessSchema guesser inp
	hClose tmpHandle
        L.writeFile tmpPath out
        handler (Just tmpPath)
