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
import Control.Monad.Trans.Class (lift)
import qualified Control.Error as E
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
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
type Analyse = L.Text -> [X.Sent P.Tag]

-- | Perform morphlogical tagging on the input text.
tag :: Analyse -> Concraft -> L.Text -> [X.Sent P.Tag]
tag analyse Concraft{..} =
    let doTag = D.include (D.disamb disamb)
              . G.include (G.guess guessNum guesser)
    in  map doTag . analyse

-- | A dataset element.  Original sentence is needed to perform
-- reanalysis of the data.
data Elem = Elem
    { sent  :: X.Sent P.Tag
    , orig  :: T.Text }

-- | Train guessing and disambiguation models.
train
    :: P.Tagset         -- ^ Tagset
    -> Analyse          -- ^ Analysis function
    -> Int              -- ^ Numer of guessed tags for each word 
    -> G.TrainConf      -- ^ Guessing model training configuration
    -> D.TrainConf      -- ^ Disambiguation model training configuration
    -> [Elem]           -- ^ Training data
    -> Maybe [Elem]     -- ^ Maybe evaluation data
    -> E.Script Concraft
train tagset ana guessNum guessConf disambConf train eval'Maybe = do
    E.scriptIO $ putStrLn "\n===== Reanalysis ====\n"
    -- TODO: Since we are catching errors here, the trainR list will be
    -- evaluated here.  We would like to avoid this, the trainR list
    -- should be generated lazily.
    trainR <- E.hoistEither $ reanalyse tagset ana train
    evalR'Maybe <- case eval'Maybe of
        Just eval   -> Just <$> E.hoistEither (reanalyse tagset ana eval)
        Nothing     -> return Nothing

    E.scriptIO $ putStrLn "\n===== Train guessing model ====\n"
    guesser <- E.scriptIO $ G.train guessConf trainR evalR'Maybe
    E.left "done"

--     let withGuesser = guessFile format guessNum guesser
--     withGuesser "train" (Just trainPath) $ \(Just trainPathG) ->
--       withGuesser "eval"   evalPath'Maybe  $ \evalPathG'Maybe  -> do
--         putStrLn "\n===== Train disambiguation model ====\n"
--         disamb <- D.train format disambConf trainPathG evalPathG'Maybe
--         return $ Concraft guessNum guesser disamb

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

-- | Reanalyse the dataset.
reanalyse :: P.Tagset -> Analyse -> [Elem] -> Either String [X.Sent P.Tag]
reanalyse tagset ana elems = do
    segments <- X.sync tagset (concat gold) (concat reana)
    return $ chunk (map length reana) segments
  where
    gold    = map sent elems
    reana   = ana $ L.fromChunks $ map orig elems

-- | Divide the list into a list of chunks given the list of
-- lengths of individual chunks.
chunk :: [Int] -> [a] -> [[a]]
chunk (n:ns) xs = 
    let (first, rest) = splitAt n xs 
    in  first : chunk ns rest
chunk [] [] = []
