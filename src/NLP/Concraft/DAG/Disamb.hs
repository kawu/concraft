{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}


module NLP.Concraft.DAG.Disamb
(
-- * Types
  Disamb (..)

-- * Tiers
, P.Tier (..)
, P.Atom (..)

-- * Marginals
, marginalsSent
, marginals

-- * Probs in general
, CRF.ProbType (..)
, probsSent
, probs

-- * Training
, TrainConf (..)
, train

-- * Pruning
, prune
) where


import Prelude hiding (words)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import Data.Text.Binary ()
import System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.List as List

import qualified Data.DAG as DAG
import           Data.DAG (DAG)

import qualified Control.Monad.Ox as Ox
import qualified Numeric.SGD as SGD
import qualified Data.CRF.Chain2.Tiers.DAG as CRF
import qualified Data.Tagset.Positional as T

-- import           NLP.Concraft.Schema hiding (schematize)
-- import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Disamb.Positional as P
import           NLP.Concraft.DAG.Schema hiding (schematize)
import qualified NLP.Concraft.DAG.Morphosyntax as X


-- | A disambiguation model.
data Disamb = Disamb
    { tiers         :: [P.Tier]
    , schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob P.Atom }


instance Binary Disamb where
    put Disamb{..} = put tiers >> put schemaConf >> put crf
    get = Disamb <$> get <*> get <*> get


--------------------------
-- Schematize
--------------------------


-- | Schematize the input sentence according to 'schema' rules.
schematize :: Schema w [t] a -> X.Sent w [t] -> CRF.Sent Ob t
schematize schema sent =
  DAG.mapE f sent
  where
    f i = const $ CRF.mkWord (obs i) (lbs i)
    obs = S.fromList . Ox.execOx . schema sent
    lbs i = X.interpsSet w
      where w = DAG.edgeLabel i sent


--------------------------
-- Marginals
--------------------------


-- | Replace the probabilities of the sentence with the marginal probabilities
-- stemming from the model.
marginalsSent :: (X.Word w) => Disamb -> X.Sent w T.Tag -> X.Sent w T.Tag
marginalsSent dmb sent = inject (marginals dmb sent) sent


-- | Determine the marginal probabilities of to individual labels in the sentence.
marginals :: (X.Word w) => Disamb -> X.Sent w T.Tag -> DAG () (X.WMap T.Tag)
marginals dmb sent
  = fmap unSplitWMap
  . DAG.zipE sent
  . _marginals dmb
  $ sent
  where
    unSplitWMap (seg, wmap) = X.mapWMap (unSplitAtoms seg) wmap
    unSplitAtoms seg = unSplit (P.split (tiers dmb)) seg


-- | Unsplit a complex tag (assuming that it is one of the interpretations of
-- the word).
unSplit :: Eq t => (r -> t) -> X.Seg w r -> t -> r
unSplit split word x = case jy of
    Just y  -> y
    Nothing -> error "unSplit: no such interpretation"
  where
    jy = List.find ((==x) . split) (X.interps word)


-- | Determine the marginal probabilities of to individual labels in the sentence.
_marginals :: (X.Word w) => Disamb -> X.Sent w T.Tag -> DAG () (X.WMap [P.Atom])
_marginals dmb
  = fmap getTags
  . marginalsCRF dmb
  where
    getTags = X.mkWMap . M.toList . choice -- CRF.unProb . snd
    -- below we mix the chosen and the potential interpretations together
    choice w = M.unionWith (+)
      (CRF.unProb . snd $ w)
      (M.fromList . map (,0) . interps $ w)
    interps = S.toList . CRF.lbs . fst


-- | Ascertain the marginal probabilities of the individual labels in the sentence.
marginalsCRF :: (X.Word w) => Disamb -> X.Sent w T.Tag -> CRF.SentL Ob P.Atom
marginalsCRF Disamb{..} sent
--   -- let schema = fromConf (schemaConf gsr)
--   -- in  CRF.marginals (crf gsr) (schematize schema sent)
--   = map (uncurry embed)
--   . zip sent
  = CRF.marginals crf
  . schematize schema
  . X.mapSent split
  $ sent
  where
    schema  = fromConf schemaConf
    split   = P.split tiers
    -- embed w = X.mkWMap . zip (X.interps w)


-- | Replace the probabilities of the sentence labels with the new probabilities
-- stemming from the CRF sentence.
inject :: DAG () (X.WMap t) -> X.Sent w t -> X.Sent w t
inject newSent srcSent =
  let doit (new, src) = src {X.tags = new}
  in  fmap doit (DAG.zipE newSent srcSent)


--------------------------
-- Probs in general
--------------------------


-- | Replace the probabilities of the sentence with the marginal probabilities
-- stemming from the model.
probsSent :: (X.Word w) => CRF.ProbType -> Disamb -> X.Sent w T.Tag -> X.Sent w T.Tag
probsSent probTyp dmb sent = inject (probs probTyp dmb sent) sent


-- | Determine the marginal probabilities of to individual labels in the sentence.
probs :: (X.Word w) => CRF.ProbType -> Disamb -> X.Sent w T.Tag -> DAG () (X.WMap T.Tag)
probs probTyp dmb sent
  = fmap unSplitWMap
  . DAG.zipE sent
  . _probs probTyp dmb
  $ sent
  where
    unSplitWMap (seg, wmap) = X.mapWMap (unSplitAtoms seg) wmap
    unSplitAtoms seg = unSplit (P.split (tiers dmb)) seg


-- | Determine the marginal probabilities of to individual labels in the sentence.
_probs :: (X.Word w) => CRF.ProbType -> Disamb -> X.Sent w T.Tag -> DAG () (X.WMap [P.Atom])
_probs probTyp dmb
  = fmap getTags
  . probsCRF probTyp dmb
  where
    getTags = X.mkWMap . M.toList . choice -- CRF.unProb . snd
    -- below we mix the chosen and the potential interpretations together
    choice w = M.unionWith (+)
      (CRF.unProb . snd $ w)
      (M.fromList . map (,0) . interps $ w)
    interps = S.toList . CRF.lbs . fst


-- | Ascertain the marginal probabilities of the individual labels in the sentence.
probsCRF :: (X.Word w) => CRF.ProbType -> Disamb -> X.Sent w T.Tag -> CRF.SentL Ob P.Atom
probsCRF probTyp Disamb{..} sent
  = CRF.probs probTyp crf
  . schematize schema
  . X.mapSent split
  $ sent
  where
    schema  = fromConf schemaConf
    split   = P.split tiers


--------------------------
-- Pruning
--------------------------


-- | Prune disamb model: discard model features with absolute values
-- (in log-domain) lower than the given threshold.
prune :: Double -> Disamb -> Disamb
prune x dmb =
    let crf' = CRF.prune x (crf dmb)
    in  dmb { crf = crf' }


--------------------------
-- Training
--------------------------


-- | Training configuration.
data TrainConf
    = TrainConf
        { tiersT        :: [P.Tier]
        , schemaConfT   :: SchemaConf
        , sgdArgsT      :: SGD.SgdArgs
        , onDiskT       :: Bool }
--     | ReTrainConf
--         { initDmb       :: Disamb
--         , sgdArgsT      :: SGD.SgdArgs
--         , onDiskT       :: Bool }


-- | Train disambiguation module.
train
    :: (X.Word w)
    => TrainConf            -- ^ Training configuration
    -> IO [X.Sent w T.Tag]  -- ^ Training data
    -> IO [X.Sent w T.Tag]  -- ^ Evaluation data
    -> IO Disamb
train TrainConf{..} trainData evalData = do
    crf <- CRF.train (length tiersT) CRF.selectHidden sgdArgsT onDiskT
        (schemed schema split <$> trainData)
        (schemed schema split <$> evalData)
    putStr "\nNumber of features: " >> print (CRF.size crf)
    return $ Disamb tiersT schemaConfT crf
  where
    schema = fromConf schemaConfT
    split  = P.split tiersT


-- | Schematized dataset.
schemed
  -- :: (X.Word w, Ord t)
  :: (Ord t)
  => Schema w [t] a
  -> (T.Tag -> [t])
  -> [X.Sent w T.Tag]
  -> [CRF.SentL Ob t]
schemed schema split =
    map onSent
  where
    onSent sent =
        let xs = fmap (X.mapSeg split) sent
            mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        -- in  fmap (uncurry CRF.mkWordL) $
        in  DAG.zipE (schematize schema xs) (fmap mkProb xs)
