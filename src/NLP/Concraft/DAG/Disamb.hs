{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}


module NLP.Concraft.DAG.Disamb
(
-- * Types
  Disamb (..)
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

-- * Internal
, schematize
) where


import Prelude hiding (words)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary (put, get, Put, Get)
import Data.Text.Binary ()
-- import System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map as M
-- import qualified Data.Vector as V
-- import qualified Data.List as List

import qualified Data.DAG as DAG
import           Data.DAG (DAG)

import qualified Control.Monad.Ox as Ox
import qualified Numeric.SGD.Momentum as SGD
import qualified Data.CRF.Chain2.Tiers.DAG as CRF
import qualified Data.Tagset.Positional as T

-- import           NLP.Concraft.Schema hiding (schematize)
-- import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Disamb.Positional as P
import           NLP.Concraft.DAG.Schema hiding (schematize)
import qualified NLP.Concraft.DAG.Morphosyntax as X


-- | A disambiguation model.
data Disamb t = Disamb
    { tiers         :: [P.Tier]
    , schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob P.Atom
    -- , simpliMap     :: M.Map t T.Tag
    , simplify      :: t -> T.Tag
      -- ^ A map which simplifies the tags of generic type `t` to simplified
      -- positional tags. The motivation behind this is that tags can have a
      -- richer structure.
      --
      -- NOTE: it can happen in real situations that a tag is encountered which
      -- is not known by the model. It would be nice to be able to treat it as
      -- the closest tag that can be handled. Then, one have to define the
      -- notion of the similarilty between tags, though... But probably it
      -- should be done at a different level (where more information about the
      -- structure of `t` is known)
    }


-- instance (Binary t) => Binary (Disamb t) where
--     put Disamb{..} = put tiers >> put schemaConf >> put crf >> put simpliMap
--     get = Disamb <$> get <*> get <*> get <*> get


-- | Store the entire disambiguation model apart from the simplification
-- function.
putDisamb :: Disamb t -> Put
putDisamb Disamb{..} =
  put tiers >> put schemaConf >> put crf


-- | Get the disambiguation model, provided the simplification function.
-- getDisamb :: (M.Map t T.Tag) -> Get (Disamb t)
getDisamb :: (t -> T.Tag) -> Get (Disamb t)
getDisamb smp =
  Disamb <$> get <*> get <*> get <*> pure smp


--------------------------
-- Simplify
--------------------------


-- -- | Simplify the given label.
-- simplify :: (Ord t) => Disamb t -> t -> T.Tag
-- simplify Disamb{..} x =
--   case M.lookup x simpliMap of
--     Nothing -> defaultTag
--     Just y -> y
--   where
--     defaultTag = snd $ M.findMin simpliMap


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


-- --------------------------
-- -- Marginals
-- --------------------------
--
--
-- -- | Determine the marginal probabilities of to individual labels in the sentence.
-- marginals :: (X.Word w, Ord t) => Disamb t -> X.Sent w t -> DAG () (X.WMap t)
-- marginals dmb = fmap X.tags . marginalsSent dmb
--
--
-- -- | Determine the marginal probabilities of to individual labels in the sentence.
-- -- marginalsSent :: (X.Word w, Ord t) => Disamb t -> X.Sent w t -> DAG () (X.WMap [P.Atom])
-- marginalsSent :: (X.Word w, Ord t) => Disamb t -> X.Sent w t -> X.Sent w t
-- marginalsSent dmb sent
--   = (\new -> inject dmb new sent)
--   . fmap getTags
--   . marginalsCRF dmb
--   $ sent
--   where
--     getTags = X.mkWMap . M.toList . choice -- CRF.unProb . snd
--     -- below we mix the chosen and the potential interpretations together
--     choice w = M.unionWith (+)
--       (CRF.unProb . snd $ w)
--       (M.fromList . map (,0) . interps $ w)
--     interps = S.toList . CRF.lbs . fst
--
--
-- -- | Ascertain the marginal probabilities of the individual labels in the sentence.
-- marginalsCRF :: (X.Word w, Ord t) => Disamb t -> X.Sent w t -> CRF.SentL Ob P.Atom
-- marginalsCRF dmb
--   = CRF.marginals (crf dmb)
--   . schematize schema
--   . X.mapSent (split . simplify dmb)
--   where
--     schema = fromConf (schemaConf dmb)
--     split  = P.split (tiers dmb)


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
      M.lookup (P.split (tiers dmb) (simplify dmb tag) Nothing) (X.unWMap newSpl) )
  | (tag, _) <- M.toList (X.unWMap src) ]


-- -- | Unsplit a complex tag (assuming that it is one of the interpretations of
-- -- the word).
-- unSplit :: Eq t => (r -> t) -> X.Seg w r -> t -> r
-- unSplit split word x = case jy of
--     Just y  -> y
--     Nothing -> error "unSplit: no such interpretation"
--   where
--     jy = List.find ((==x) . split) (X.interps word)


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
  . X.mapSent (split . simplify dmb)
  where
    schema = fromConf (schemaConf dmb)
    split  = \t -> P.split (tiers dmb) t Nothing


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
  , simplifyLabel :: t -> T.Tag
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
    (schemed simplifyLabel schema split <$> trainData)
    (schemed simplifyLabel schema split <$> evalData)
  putStr "\nNumber of features: " >> print (CRF.size crf)
  return $ Disamb tiersT schemaConfT crf simplifyLabel -- tagMap
  where
    schema = fromConf schemaConfT
    split  = \t -> P.split tiersT t Nothing


-- | Schematized dataset.
schemed
  -- :: (X.Word w, Ord t)
  :: (Ord a)
  => (t -> T.Tag)
  -> Schema w [a] b
  -> (T.Tag -> [a])
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
