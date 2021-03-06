{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}


module NLP.Concraft.DAG.Guess
(
-- * Types
  Guesser (..)
, putGuesser
, getGuesser

-- * Marginals
, marginals
, marginalsSent

-- -- * Guessing
-- , guess
-- , include
-- , guessSent

-- * Training
, TrainConf (..)
, R0T (..)
, train

-- * Utils
, schemed
) where


import Prelude hiding (words)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary (Binary, put, get, Put, Get)
import Data.Text.Binary ()
import System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map.Strict as M
-- import qualified Data.Vector as V

import qualified Data.DAG as DAG
import           Data.DAG (DAG)

import qualified Control.Monad.Ox as Ox
-- import qualified Data.CRF.Chain1.Constrained as CRF
import qualified Data.CRF.Chain1.Constrained.DAG as CRF
-- import qualified Data.CRF.Chain1.Constrained.Dataset.External as CRF.Ext
import qualified Numeric.SGD.Momentum as SGD

-- import           NLP.Concraft.Schema hiding (schematize)
-- import qualified NLP.Concraft.Morphosyntax as X
import           NLP.Concraft.DAG.Schema hiding (schematize)
import qualified NLP.Concraft.DAG.Morphosyntax as X


-- | A guessing model.
data Guesser t s = Guesser
    { schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob s
    , zeroProbLab   :: s
    , unkTagSet     :: S.Set t
      -- ^ The tagset considered for the unknown words (TODO: a solution
      -- parallel and not 100% consistent with what is implemented in the CRF
      -- library)
      -- TODO: with `complexify`, `unkTagSet` is not needed anymore!
    , simplify      :: t -> s
      -- ^ A tag simplification function
    , complexify    :: s -> t
      -- ^ NEW: instead of an `unkTagSet`, a function which makes a complex tag
      -- out of a simple tag.
      --
      -- WARNING: we assume, that this function does not conflate simplified
      -- tags, i.e., tag to tags of type `s` cannot lead to one and the same
      -- complex tag of type `t`.
    }


-- instance (Ord t, Binary t, Ord s, Binary s) => Binary (Guesser t s) where
--     put Guesser{..} = do
--       put schemaConf
--       put crf
--       put zeroProbLab
--       put simpliMap
--     get = Guesser <$> get <*> get <*> get <*> get


-- | Store the entire guessing model apart from the simplification function.
putGuesser :: (Binary t, Binary s, Ord s) => Guesser t s -> Put
putGuesser Guesser{..} = do
  put schemaConf
  put crf
  put zeroProbLab
  put unkTagSet


-- | Get the disambiguation model, provided the simplification function.
-- getGuesser :: (M.Map t T.Tag) -> Get (Guesser t)
getGuesser ::
     (Binary t, Binary s, Ord s, Ord t)
  => (t -> s)
  -> (s -> t)
  -> Get (Guesser t s)
getGuesser smp cpx =
  Guesser <$> get <*> get <*> get <*> get <*> pure smp <*> pure cpx


-- --------------------------
-- -- Simplify
-- --------------------------
--
--
-- -- | Simplify the given label.
-- simplify :: (Ord t) => Guesser t s -> t -> s
-- simplify Guesser{..} x =
--   case M.lookup x simpliMap of
--     Nothing -> zeroProbLab
--     Just y -> y


--------------------------
-- Schematize
--------------------------


-- | Schematize the input sentence according to the 'schema' rules.
-- TODO: looks like there is no reason at all for `Schema w t a` to
-- be parametrized with `t`?
schematize :: (X.Word w) => Schema w t a -> X.Sent w t -> CRF.Sent Ob t
schematize schema sent =
  DAG.mapE f sent
  where
    f i = const $ CRF.Word (obs i) (lbs i)
    obs = S.fromList . Ox.execOx . schema sent
    lbs i
      | X.oov w  = S.empty
      | otherwise = X.interpsSet w
      where w = DAG.edgeLabel i sent


--------------------------
-- Marginals
--------------------------


-- | Determine the marginal probabilities of the individual labels in the sentence.
marginals
  :: (X.Word w, Ord t, Ord s)
  => CRF.Config s
  -> Guesser t s
  -> X.Sent w t
  -> DAG () (X.WMap t)
marginals cfg gsr = fmap X.tags . marginalsSent cfg gsr


-- | Replace the probabilities of the sentence labels with the marginal probabilities
-- stemming from the model.
marginalsSent
  :: (X.Word w, Ord t, Ord s)
  => CRF.Config s
  -> Guesser t s
  -> X.Sent w t
  -> X.Sent w t
marginalsSent cfg gsr sent
  = (\new -> inject gsr new sent)
  . fmap tags
  . marginalsCRF cfg gsr
  $ sent
  where
    tags = X.fromMap . considerZero . choice
    -- we mix the chosen and the potential interpretations together
    choice w = M.unionWith (+)
      (CRF.unProb . CRF.choice $ w)
      -- (M.fromList . map (,0) . interps $ w)
      (M.fromSet (const 0) . interps $ w)
    interps = CRF.lbs . CRF.word
    -- if empty, we choose the zero probability label.
    considerZero m
      | M.null m = M.singleton (zeroProbLab gsr) 0
      | otherwise = m


-- | Ascertain the marginal probabilities of to individual labels in the sentence.
marginalsCRF
  :: (X.Word w, Ord t, Ord s)
  => CRF.Config s
  -> Guesser t s
  -> X.Sent w t
  -> CRF.SentL Ob s
marginalsCRF cfg gsr dag0 =
  let schema = fromConf (schemaConf gsr)
      dag = X.mapSent (simplify gsr) dag0
  in  CRF.marginals cfg (crf gsr) (schematize schema dag)


-- -- | Replace the probabilities of the sentence labels with the new probabilities
-- -- stemming from the CRF sentence.
-- inject :: DAG () (X.WMap t) -> X.Sent w t -> X.Sent w t
-- inject newSent srcSent =
--   let doit (new, src) = src {X.tags = new}
--   in  fmap doit (DAG.zipE newSent srcSent)


-- | Replace the probabilities of the sentence labels with the new probabilities
-- stemming from the CRF sentence.
--
-- TODO: The behavior for OOV words seems unoptimal, since all possible labels
-- are taken into account, and not only the default CRF ones.  Still, it's not
-- necessarily a problem, maybe not even from the speed point of view.
--
inject
  :: (Ord t, Ord s, X.Word w)
  => Guesser t s
  -> DAG () (X.WMap s)
  -> X.Sent w t
  -> X.Sent w t
inject gsr newSent srcSent =
  let doit (target, src)
        | X.oov (X.word src) =
            let newTags = X.fromMap $ M.fromAscList
                  [ (complexify gsr tag, prob) 
                  | (tag, prob) <- M.toAscList (X.unWMap target) ]
            in  src {X.tags = newTags}
        | otherwise =
            let oldTags = X.tags src
                newTags = injectWMap gsr target oldTags
            in  src {X.tags = newTags}
  in  fmap doit (DAG.zipE newSent srcSent)


-- -- | Replace label probabilities with the new probabilities.
-- inject
--   :: (Ord t, Ord s)
--   => Guesser t s
--   -> DAG () (X.WMap s)
--   -> DAG () (X.WMap t)
--   -> DAG () (X.WMap t)
-- inject gsr newDat srcDag =
--   let doit (newSpl, src) = injectWMap gsr newSpl src
--   in  fmap doit (DAG.zipE newDat srcDag)


-- | Replace label probabilities with the new probabilities.
injectWMap
  :: (Ord t, Ord s)
  => Guesser t s
  -> X.WMap s
  -> X.WMap t
  -> X.WMap t
injectWMap gsr newSpl src = X.mkWMap
  [ ( tag
    , maybe 0 id $
      M.lookup (simplify gsr tag) (X.unWMap newSpl) )
  | (tag, _) <- M.toList (X.unWMap src) ]
-- injectWMap gsr newSpl src = X.fromMap $ M.fromAscList
--   [ (complexify gsr smp, prob)
--   | (smp, prob) <- M.toAscList (X.unWMap newSpl) ]


-- --------------------------
-- -- ???
-- --------------------------
--
--
--
-- -- -- | Determine the 'k' most probable labels for each word in the sentence.
-- -- guess :: (X.Word w, Ord t)
-- --       => Int -> Guesser t -> X.Sent w t -> [[t]]
-- -- guess k gsr sent =
-- --     let schema = fromConf (schemaConf gsr)
-- --     in  CRF.tagK k (crf gsr) (schematize schema sent)
--
--
-- -- -- | Insert guessing results into the sentence.  Only interpretations
-- -- -- of OOV words will be extended.
-- -- include :: (X.Word w, Ord t) => [[t]] -> X.Sent w t -> X.Sent w t
-- -- include xss sent =
-- --     [ word { X.tags = tags }
-- --     | (word, tags) <- zip sent sentTags ]
-- --   where
-- --     sentTags =
-- --         [ if X.oov word
-- --             then addInterps (X.tags word) xs
-- --             else X.tags word
-- --         | (xs, word) <- zip xss sent ]
-- --     addInterps wm xs = X.mkWMap
-- --         $  M.toList (X.unWMap wm)
-- --         ++ zip xs [0, 0 ..]
-- --
-- --
-- -- -- | Combine `guess` with `include`.
-- -- guessSent :: (X.Word w, Ord t)
-- --           => Int -> Guesser t
-- --           -> X.Sent w t -> X.Sent w t
-- -- guessSent guessNum guesser sent =
-- --     include (guess guessNum guesser sent) sent
-- --


--------------------------
-- Training
--------------------------


-- | Method of constructing the default set of labels (R0).
data R0T
    = AnyInterps        -- ^ See `CRF.anyInterps`
    | AnyChosen         -- ^ See `CRF.anyChosen`
    | OovChosen         -- ^ See `CRF.oovChosen`
    deriving (Show, Eq, Ord, Enum, Typeable, Data)


-- | Training configuration.
data TrainConf t s = TrainConf
    { schemaConfT   :: SchemaConf
    -- | SGD parameters.
    , sgdArgsT      :: SGD.SgdArgs
    -- | Store SGD dataset on disk
    , onDiskT       :: Bool
    -- | R0 construction method
    , r0T           :: R0T
    -- | Zero probability label
    , zeroProbLabel :: t
    -- | Label simplification function
    , simplifyLabel :: t -> s
    -- | Label complexification function
    , complexifyLabel :: s -> t
    -- | Strip the label from irrelevant information.  Used to determine the
    -- set of possible tags for unknown words.
    -- TODO: we don't need this with `complexify` anymore!?
    , stripLabel :: t -> t
    -- | Guess only visible features
    , onlyVisible :: Bool
    }


-- | Train guesser.
train
    :: (X.Word w, Ord t, Ord s)
    => TrainConf t s        -- ^ Training configuration
    -> IO [X.Sent w t]      -- ^ Training data
    -> IO [X.Sent w t]      -- ^ Evaluation data
    -> IO (Guesser t s)
train TrainConf{..} trainData evalData = do
  let schema = fromConf schemaConfT
      mkR0   = case r0T of
        AnyInterps  -> CRF.anyInterps
        AnyChosen   -> CRF.anyChosen
        OovChosen   -> CRF.oovChosen
      featExtract =
        if onlyVisible
        then const CRF.presentFeats
        else \r0 -> CRF.hiddenFeats r0 . map (fmap fst)
  tagSet <- S.unions . map (tagSetIn stripLabel) <$> trainData
--   let tagMap = M.fromList
--         [ (t, simplifyLabel t)
--         | t <- S.toList tagSet ]
  crf <- CRF.train sgdArgsT onDiskT
    -- mkR0 (const CRF.presentFeats)
    -- mkR0 (\r0 -> CRF.hiddenFeats r0 . map (fmap fst))
    mkR0 featExtract
    (schemed simplifyLabel schema <$> trainData)
    (schemed simplifyLabel schema <$> evalData)
  return $
    Guesser
      schemaConfT
      crf
      (simplifyLabel zeroProbLabel)
      tagSet
      simplifyLabel
      complexifyLabel


-- | Schematized dataset.
schemed
  :: (X.Word w, Ord t, Ord s)
  => (t -> s)
  -> Schema w s a
  -> [X.Sent w t]
  -> [CRF.SentL Ob s]
schemed simpl schema =
    map onSent
  where
    onSent dag0 =
        let dag = X.mapSent simpl dag0
            mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        in  fmap (uncurry CRF.mkWordL) $
            DAG.zipE (schematize schema dag) (fmap mkProb dag)


-- | Retrieve the tagset in the given sentence, provided the stripping function
-- (see `stripLabel`).
tagSetIn :: (Ord t) => (t -> t) -> X.Sent w t -> S.Set t
tagSetIn strip dag = S.fromList
  [ strip tag
  | edgeID <- DAG.dagEdges dag
  , let edge = DAG.edgeLabel edgeID dag
  , tag <- M.keys . X.unWMap . X.tags $ edge ]


-- | Compute the default `X.WMap` for unknown tags.
compUnkWMap :: Ord t => S.Set t -> X.WMap t
compUnkWMap = X.mkWMap . map (,0) . S.toList
