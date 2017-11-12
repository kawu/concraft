{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}


-- | A version of the disambigation model adapted to perform sentence
-- segmentation as well.


module NLP.Concraft.DAG.DisambSeg
(
-- * Types
  Tag (..)
, Disamb (..)
, putDisamb
, getDisamb

-- * Tiers
, P.Tier (..)
, P.Atom (..)

-- -- * Marginals
-- , marginalsSent
-- , marginals

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
import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary (Binary, put, get, Put, Get)
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

import NLP.Concraft.DAG.Disamb (schematize)


-- | The internal tag type.
data Tag = Tag
  { posiTag :: T.Tag
    -- ^ Positional tag
  , hasEos :: Bool
    -- ^ End-of-sentence marker
  } deriving (Show, Eq, Ord)


-- | A disambiguation model.
data Disamb t = Disamb
    { tiers         :: [P.Tier]
    , schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob P.Atom
    , simplify      :: t -> Tag
      -- ^ A function which simplifies the tags of the generic type `t` to (i)
      -- the corresponding positional tags and (ii) information if the segment
      -- represents sentence end.
      --
      -- NOTE: it can happen in real situations that a tag is encountered which
      -- is not known by the model. It would be nice to be able to treat it as
      -- the closest tag that can be handled. Then, one have to define the
      -- notion of the similarilty between tags, though... But probably it
      -- should be done at a different level (where more information about the
      -- structure of `t` is known)
    }


-- | Store the entire disambiguation model apart from the simplification
-- function.
putDisamb :: Disamb t -> Put
putDisamb Disamb{..} =
  put tiers >> put schemaConf >> put crf


-- | Get the disambiguation model, provided the simplification function.
-- getDisamb :: (M.Map t T.Tag) -> Get (Disamb t)
getDisamb :: (t -> Tag) -> Get (Disamb t)
getDisamb smp =
  Disamb <$> get <*> get <*> get <*> pure smp


--------------------------
-- Injection
--------------------------


-- | Replace the probabilities of the sentence labels with the new probabilities
-- stemming from the CRF sentence.
inject
  :: (Ord t, X.Word w)
  => Disamb t
  -> DAG () (X.WMap [P.Atom])
  -> X.Sent w t
  -> X.Sent w t
inject dmb newSent srcSent =
  let doit (target, src) =
        let oldTags = X.tags src
            newTags = injectWMap dmb target oldTags
        in  src {X.tags = newTags}
  in  fmap doit (DAG.zipE newSent srcSent)


-- | Replace label probabilities with the new probabilities.
injectWMap
  :: (Ord t)
  => Disamb t
  -> X.WMap [P.Atom]
  -> X.WMap t
  -> X.WMap t
injectWMap dmb newSpl src = X.mkWMap
  [ ( tag
    , maybe 0 id $
      M.lookup (split (tiers dmb) (simplify dmb tag)) (X.unWMap newSpl) )
  | (tag, _) <- M.toList (X.unWMap src) ]


--------------------------
-- Probs in general
--------------------------


-- | Determine the marginal probabilities of to individual labels in the sentence.
probs :: (X.Word w, Ord t) => CRF.ProbType -> Disamb t -> X.Sent w t -> DAG () (X.WMap t)
probs probTyp dmb = fmap X.tags . probsSent probTyp dmb


-- | Determine the marginal probabilities of to individual labels in the sentence.
-- marginalsSent :: (X.Word w, Ord t) => Disamb t -> X.Sent w t -> DAG () (X.WMap [P.Atom])
probsSent :: (X.Word w, Ord t) => CRF.ProbType -> Disamb t -> X.Sent w t -> X.Sent w t
probsSent probTyp dmb sent
  = (\new -> inject dmb new sent)
  . fmap getTags
  . probsCRF probTyp dmb
  $ sent
  where
    getTags = X.mkWMap . M.toList . choice -- CRF.unProb . snd
    -- below we mix the chosen and the potential interpretations together
    choice w = M.unionWith (+)
      (CRF.unProb . snd $ w)
      (M.fromList . map (,0) . interps $ w)
    interps = S.toList . CRF.lbs . fst




-- | Ascertain the marginal probabilities of the individual labels in the sentence.
probsCRF :: (X.Word w, Ord t) => CRF.ProbType -> Disamb t -> X.Sent w t -> CRF.SentL Ob P.Atom
probsCRF probTyp dmb
  = CRF.probs probTyp (crf dmb)
  . schematize schema
  . X.mapSent (split (tiers dmb) . simplify dmb)
  where
    schema = fromConf (schemaConf dmb)


--------------------------
-- Pruning
--------------------------


-- | Prune disamb model: discard model features with absolute values
-- (in log-domain) lower than the given threshold.
prune :: Double -> Disamb t -> Disamb t
prune x dmb =
    let crf' = CRF.prune x (crf dmb)
    in  dmb { crf = crf' }


--------------------------
-- Training
--------------------------


-- | Training configuration.
data TrainConf t = TrainConf
  { tiersT      :: [P.Tier]
  , schemaConfT :: SchemaConf
  , sgdArgsT    :: SGD.SgdArgs
  , onDiskT     :: Bool
  -- | Label simplification function
  , simplifyLabel :: t -> Tag
  }


-- | Train disambiguation module.
train
    :: (X.Word w, Ord t)
    => TrainConf t      -- ^ Training configuration
    -> IO [X.Sent w t]  -- ^ Training data
    -> IO [X.Sent w t]  -- ^ Evaluation data
    -> IO (Disamb t)
train TrainConf{..} trainData evalData = do
--   tagSet <- S.unions . map tagSetIn <$> trainData
--   putStr "\nTagset size: " >> print (S.size tagSet)
--   let tagMap = M.fromList
--         [ (t, simplifyLabel t)
--         | t <- S.toList tagSet ]
  crf <- CRF.train (length tiersT) CRF.selectHidden sgdArgsT onDiskT
    (schemed simplifyLabel schema (split tiersT) <$> trainData)
    (schemed simplifyLabel schema (split tiersT) <$> evalData)
  putStr "\nNumber of features: " >> print (CRF.size crf)
  return $ Disamb tiersT schemaConfT crf simplifyLabel -- tagMap
  where
    schema = fromConf schemaConfT


-- | Schematized dataset.
schemed
  -- :: (X.Word w, Ord t)
  :: (Ord a)
  => (t -> Tag)
  -> Schema w [a] b
  -> (Tag -> [a])
  -> [X.Sent w t]
  -> [CRF.SentL Ob a]
schemed simpl schema split =
    map onSent
  where
    onSent sent =
        let xs = fmap (X.mapSeg split) (X.mapSent simpl sent)
            mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        -- in  fmap (uncurry CRF.mkWordL) $
        in  DAG.zipE (schematize schema xs) (fmap mkProb xs)


-- -- | Retrieve the tagset in the given sentence.
-- tagSetIn :: (Ord t) => X.Sent w t -> S.Set t
-- tagSetIn dag = S.fromList
--   [ tag
--   | edgeID <- DAG.dagEdges dag
--   , let edge = DAG.edgeLabel edgeID dag
--   , tag <- M.keys . X.unWMap . X.tags $ edge ]



--------------------------
-- Utils
--------------------------


-- | Split the tag with respect to the given tiers.
split :: [P.Tier] -> Tag -> [P.Atom]
split tiers tag = P.split tiers (posiTag tag) (Just $ hasEos tag)
