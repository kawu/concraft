{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Guess
(
-- * Types
  Guesser (..)
 
-- * Guessing
, guess
, include
, guessSent

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
schematize :: (X.HasOOV w, Ord t) => Schema w t a -> X.Sent w t -> CRF.Sent Ob t
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
guess :: (X.HasOOV w, X.HasOrth w, Ord t)
      => Int -> Guesser t -> X.Sent w t -> [[t]]
guess k gsr sent =
    let schema = fromConf (schemaConf gsr)
    in  CRF.tagK k (crf gsr) (schematize schema sent)

-- | Insert guessing results into the sentence.
include :: (X.HasOOV w, Ord t) => (X.Sent w t -> [[t]])
        -> X.Sent w t -> X.Sent w t
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

-- | Combine `guess` with `include`. 
guessSent :: (X.HasOOV w, X.HasOrth w, Ord t)
          => Int -> Guesser t -> X.Sent w t -> X.Sent w t
guessSent guessNum guesser = include (guess guessNum guesser)

-- | Training configuration.
data TrainConf = TrainConf
    { schemaConfT   :: SchemaConf
    , sgdArgsT      :: SGD.SgdArgs }

-- | Train guesser.
train
    :: (X.HasOrth w, X.HasOOV w, Ord t)
    => TrainConf            -- ^ Training configuration
    -> [X.Sent w t]         -- ^ Training data
    -> Maybe [X.Sent w t]   -- ^ Maybe evaluation data
    -> IO (Guesser t)
train TrainConf{..} trainData evalData'Maybe = do
    let schema = fromConf schemaConfT
    crf <- CRF.train sgdArgsT
        (retSchemed schema trainData)
        (retSchemed schema <$> evalData'Maybe)
        (const CRF.presentFeats)
    return $ Guesser schemaConfT crf
  where
    retSchemed schema = return . schemed schema

-- | Schematized data from the plain file.
schemed :: (X.HasOOV w, Ord t) => Schema w t a
        -> [X.Sent w t] -> [CRF.SentL Ob t]
schemed schema =
    map onSent
  where
    onSent xs =
        let mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        in  zip (schematize schema xs) (map mkProb xs)
