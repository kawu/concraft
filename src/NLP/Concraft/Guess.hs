{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Concraft.Guess
( Guesser (..)
, guess
, include
, guessSent
, guessDoc
, trainOn
) where

import Prelude hiding (words)
import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.Foldable (Foldable, foldMap)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Data.CRF.Chain1.Constrained as CRF
import qualified Numeric.SGD as SGD

import NLP.Concraft.Schema
import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Ord t => Schema t a -> Mx.Sent t -> CRF.Sent Ob t
schematize schema sent =
    [ CRF.Word (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs i 
        | Mx.oov w  = S.empty
        | otherwise = Mx.interpsSet w
        where w = v V.! i

-- | A guesser represented by the conditional random field.
newtype Guesser t = Guesser { crf :: CRF.CRF Ob t }
    deriving (Binary)

-- | Determine the 'k' most probable labels for each word in the sentence.
guess :: Ord t => Int -> Schema t a -> Guesser t -> Mx.Sent t -> [[t]]
guess k schema gsr sent = CRF.tagK k (crf gsr) (schematize schema sent)

-- | Include guessing results into weighted tag maps
-- assigned to individual words.
includeWMaps :: Ord t => Mx.Sent t -> [[t]] -> [Mx.WMap t]
includeWMaps words guessed =
    [ if Mx.oov word
        then addInterps (Mx.tagWMap word) xs
        else Mx.tagWMap word
    | (xs, word) <- zip guessed words ]
  where
    -- Add new interpretations.
    addInterps wm xs = Mx.mkWMap
        $  M.toList (Mx.unWMap wm)
        ++ zip xs [0, 0 ..]

-- | Include guessing results into the sentence.
include :: Ord t => Mx.Sent t -> [[t]] -> Mx.Sent t
include words guessed =
    [ word { Mx.tagWMap = wMap }
    | (word, wMap) <- zip words wMaps ]
  where
    wMaps = includeWMaps words guessed

-- | Tag sentence in external format.  Selected interpretations
-- (tags correct within the context) will be preserved.
guessSent :: F.Sent s w -> Int -> Schema F.Tag a -> Guesser F.Tag -> s -> s
guessSent F.Sent{..} k schema gsr sent = flip mergeSent sent
    [ select wMap word
    | (wMap, word) <- zip wMaps (parseSent sent) ]
  where
    -- Extract word handler.
    F.Word{..} = wordHandler
    -- Word in internal format.
    words   = map extract (parseSent sent)
    -- Guessed lists of interpretations for individual words.
    guessed = guess k schema gsr words
    -- Resultant weighted maps. 
    wMaps   = includeWMaps words guessed

-- | Tag file.
guessDoc
    :: Functor f
    => F.Doc f s w  	-- ^ Document format handler
    -> Int              -- ^ Guesser argument
    -> Schema F.Tag a	-- ^ Observation schema
    -> Guesser F.Tag    -- ^ Guesser itself
    -> L.Text           -- ^ Input
    -> L.Text           -- ^ Output
guessDoc F.Doc{..} k schema gsr
    = showDoc 
    . fmap (guessSent sentHandler k schema gsr)
    . parseDoc

-- | Train guesser.
trainOn
    :: Foldable f
    => F.Doc f s w      -- ^ Document format handler
    -> Schema F.Tag a	-- ^ Observation schema
    -> SGD.SgdArgs      -- ^ SGD parameters 
    -> FilePath         -- ^ Training file
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO (Guesser F.Tag)
trainOn format schema sgdArgs trainPath evalPath'Maybe = do
    _crf <- CRF.train sgdArgs
        (schemed format schema trainPath)
        (schemed format schema <$> evalPath'Maybe)
        (const CRF.presentFeats)
    return $ Guesser _crf

-- | Schematized data from the plain file.
schemed
    :: Foldable f => F.Doc f s w -> Schema F.Tag a
    -> FilePath -> IO [CRF.SentL Ob F.Tag]
schemed F.Doc{..} schema path =
    foldMap onSent . parseDoc <$> L.readFile path
  where
    F.Sent{..} = sentHandler
    F.Word{..} = wordHandler
    onSent sent =
        let xs = map extract (parseSent sent)
            mkProb = CRF.mkProb . M.toList . Mx.unWMap . Mx.tagWMap
        in  [zip (schematize schema xs) (map mkProb xs)]
