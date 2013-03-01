{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Guess
(
-- * Types
  Guesser (..)
 
-- * Guessing
, guess
, include

-- * Training
, TrainConf (..)
, train
) where

import Prelude hiding (words)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Data.CRF.Chain1.Constrained as CRF
import qualified Numeric.SGD as SGD

import NLP.Concraft.Schema hiding (schematize)
import qualified NLP.Concraft.Morphosyntax as X

-- | A guessing model.
data Guesser t = Guesser
    { schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob t }

instance (Ord t, Binary t) => Binary (Guesser t) where
    put Guesser{..} = put schemaConf >> put crf
    get = Guesser <$> get <*> get

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Ord t => Schema t a -> X.Sent t -> CRF.Sent Ob t
schematize schema sent =
    [ CRF.Word (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs i 
        | X.oov w  = S.empty
        | otherwise = X.interpsSet w
        where w = v V.! i

-- | Determine 'k' most probable labels for each word in the sentence.
guess :: Ord t => Int -> Guesser t -> X.Sent t -> [[t]]
guess k gsr sent =
    let schema = fromConf (schemaConf gsr)
    in  CRF.tagK k (crf gsr) (schematize schema sent)

-- | Insert guessing results into the sentence.
include :: Ord t => (X.Sent t -> [[t]]) -> X.Sent t -> X.Sent t
include f sent =
    [ word { X.tags = tags }
    | (word, tags) <- zip sent sentTags ]
  where
    sentTags =
        [ if X.oov word
            then addInterps (X.tags word) xs
            else X.tags word
        | (xs, word) <- zip (f sent) sent ]
    addInterps wm xs = X.mkWMap
        $  M.toList (X.unWMap wm)
        ++ zip xs [0, 0 ..]

-- | Training configuration.
data TrainConf = TrainConf
    { schemaConfT   :: SchemaConf
    , sgdArgsT      :: SGD.SgdArgs }

-- | Train guesser.
train
    :: Ord t
    => (FilePath -> IO [X.Sent t])  -- ^ Data parsing IO action
    -> TrainConf                    -- ^ Training configuration
    -> FilePath                     -- ^ Training file
    -> Maybe FilePath               -- ^ Maybe eval file
    -> IO (Guesser t)
train parseIO TrainConf{..} trainPath evalPath'Maybe = do
    let schema = fromConf schemaConfT
    crf <- CRF.train sgdArgsT
        (schemed parseIO schema trainPath)
        (schemed parseIO schema <$> evalPath'Maybe)
        (const CRF.presentFeats)
    return $ Guesser schemaConfT crf

-- | Schematized data from the plain file.
schemed
    :: Ord t => (FilePath -> IO [X.Sent t])
    -> Schema t a -> FilePath -> IO [CRF.SentL Ob t]
schemed parseIO schema path =
    map onSent <$> parseIO path
  where
    onSent xs =
        let mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        in  zip (schematize schema xs) (map mkProb xs)
