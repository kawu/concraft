{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}


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
, R0T (..)
, train
) where


import Prelude hiding (words)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import Data.Text.Binary ()
import System.Console.CmdArgs
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
schematize :: (X.Word w, Ord t) => Schema w t a -> X.Sent w t -> CRF.Sent Ob t
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


-- | Determine the 'k' most probable labels for each word in the sentence.
-- TODO: Perhaps it would be better to use sets instead of lists
-- as output?
guess :: (X.Word w, Ord t)
      => Int -> Guesser t -> X.Sent w t -> [[t]]
guess k gsr sent =
    let schema = fromConf (schemaConf gsr)
    in  CRF.tagK k (crf gsr) (schematize schema sent)


-- | Insert guessing results into the sentence.
include :: (X.Word w, Ord t) => [[t]] -> X.Sent w t -> X.Sent w t
include xss sent =
    [ word { X.tags = tags }
    | (word, tags) <- zip sent sentTags ]
  where
    sentTags =
        [ if X.oov word
            then addInterps (X.tags word) xs
            else X.tags word
        | (xs, word) <- zip xss sent ]
    addInterps wm xs = X.mkWMap
        $  M.toList (X.unWMap wm)
        ++ zip xs [0, 0 ..]


-- | Combine `guess` with `include`. 
guessSent :: (X.Word w, Ord t)
          => Int -> Guesser t -> X.Sent w t -> X.Sent w t
guessSent guessNum guesser sent =
    include (guess guessNum guesser sent) sent


-- | Method of constructing the default set of labels (R0).
data R0T
    = AnyInterps        -- ^ See `CRF.anyInterps` 
    | AnyChosen         -- ^ See `CRF.anyChosen`
    | OovChosen         -- ^ See `CRF.oovChosen`
    deriving (Show, Eq, Ord, Enum, Typeable, Data)


-- | Training configuration.
data TrainConf = TrainConf
    { schemaConfT   :: SchemaConf
    -- | SGD parameters.
    , sgdArgsT      :: SGD.SgdArgs
    -- | Store SGD dataset on disk
    , onDiskT       :: Bool
    -- | R0 construction method
    , r0T           :: R0T }


-- | Train guesser.
train
    :: (X.Word w, Ord t)
    => TrainConf            -- ^ Training configuration
    -> IO [X.Sent w t]      -- ^ Training data
    -> IO [X.Sent w t]      -- ^ Evaluation data
    -> IO (Guesser t)
train TrainConf{..} trainData evalData = do
    let schema = fromConf schemaConfT
        mkR0   = case r0T of
            AnyInterps  -> CRF.anyInterps
            AnyChosen   -> CRF.anyChosen
            OovChosen   -> CRF.oovChosen
    crf <- CRF.train sgdArgsT onDiskT
        mkR0 (const CRF.presentFeats)
        (schemed schema <$> trainData)
        (schemed schema <$> evalData)
    return $ Guesser schemaConfT crf


-- | Schematized dataset.
schemed :: (X.Word w, Ord t) => Schema w t a
        -> [X.Sent w t] -> [CRF.SentL Ob t]
schemed schema =
    map onSent
  where
    onSent xs =
        let mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        in  map (uncurry CRF.mkWordL) $
            zip (schematize schema xs) (map mkProb xs)
