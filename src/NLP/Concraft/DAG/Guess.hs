{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}


module NLP.Concraft.DAG.Guess
(
-- * Types
  Guesser (..)

-- * Marginals
, marginals
-- , marginalsSent

-- -- * Guessing
-- , guess
-- , include
-- , guessSent

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
import qualified Data.Map.Strict as M
-- import qualified Data.Vector as V

import qualified Data.DAG as DAG
import           Data.DAG (DAG)

import qualified Control.Monad.Ox as Ox
-- import qualified Data.CRF.Chain1.Constrained as CRF
import qualified Data.CRF.Chain1.Constrained.DAG as CRF
-- import qualified Data.CRF.Chain1.Constrained.Dataset.External as CRF.Ext
import qualified Numeric.SGD as SGD

-- import           NLP.Concraft.Schema hiding (schematize)
-- import qualified NLP.Concraft.Morphosyntax as X
import           NLP.Concraft.DAG.Schema hiding (schematize)
import qualified NLP.Concraft.DAG.Morphosyntax as X


-- | A guessing model.
data Guesser t s = Guesser
    { schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob s
    , zeroProbLab   :: s
      -- ^ Zero probability label is used when the model does not propose any
      -- interpretation with non-zero probability for an unknown word.
    , simpliMap     :: M.Map t s
      -- ^ A map which simplifies the tags of type `t` to simplified tags of
      -- type `s`.
    }


instance (Ord t, Binary t, Ord s, Binary s) => Binary (Guesser t s) where
    put Guesser{..} = do
      put schemaConf
      put crf
      put zeroProbLab
      put simpliMap
    get = Guesser <$> get <*> get <*> get <*> get


--------------------------
-- Schematize
--------------------------


-- -- | Schematize the input sentence according to the 'schema' rules.
-- schematize :: (X.Word w) => Schema w t a -> X.Sent w t -> CRF.Sent Ob t
-- schematize schema sent =
--   DAG.mapE f sent
--   where
--     f i = const $ CRF.Word (obs i) (lbs i)
--     obs = S.fromList . Ox.execOx . schema sent
--     lbs i
--       | X.oov w  = S.empty
--       | otherwise = X.interpsSet w
--       where w = DAG.edgeLabel i sent


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


-- -- | Replace the probabilities of the sentence with the marginal probabilities
-- -- stemming from the model.
-- marginalsSent
--   :: (X.Word w, Ord s)
--   => Guesser t s
--   -> X.Sent w t
--   -> X.Sent w t
-- marginalsSent gsr sent = inject (marginals gsr sent) sent


-- | Determine the marginal probabilities of the individual labels in the sentence.
marginals :: (X.Word w, Ord t, Ord s) => Guesser t s -> X.Sent w t -> DAG () (X.WMap t)
marginals gsr = undefined
--   fmap tags . marginalsCRF gsr
--   where
--     tags = X.mkWMap . M.toList . considerZero . choice
--     -- we mix the chosen and the potential interpretations together
--     choice w = M.unionWith (+)
--       (CRF.unProb . CRF.choice $ w)
--       (M.fromList . map (,0) . interps $ w)
--     interps = S.toList . CRF.lbs . CRF.word
--     -- if empty, we choose the zero probability label.
--     considerZero m
--       | M.null m = M.singleton (zeroProbLab gsr) 0
--       | otherwise = m


-- -- | Ascertain the marginal probabilities of to individual labels in the sentence.
-- marginalsCRF :: (X.Word w, Ord t) => Guesser t -> X.Sent w t -> CRF.SentL Ob t
-- marginalsCRF gsr sent =
--   let schema = fromConf (schemaConf gsr)
--   in  CRF.marginals (crf gsr) (schematize schema sent)
--
--
-- -- | Replace the probabilities of the sentence labels with the new probabilities
-- -- stemming from the CRF sentence.
-- inject :: DAG () (X.WMap t) -> X.Sent w t -> X.Sent w t
-- inject newSent srcSent =
--   let doit (new, src) = src {X.tags = new}
--   in  fmap doit (DAG.zipE newSent srcSent)
--
--
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
  tagSet <- S.unions . map tagSetIn <$> trainData
  let tagMap = M.fromList
        [ (t, simplifyLabel t)
        | t <- S.toList tagSet ]
  crf <- CRF.train sgdArgsT onDiskT
    -- mkR0 (const CRF.presentFeats)
    mkR0 (\r0 -> CRF.hiddenFeats r0 . map (fmap fst))
    (schemed simplifyLabel schema <$> trainData)
    (schemed simplifyLabel schema <$> evalData)
  return $ Guesser schemaConfT crf (simplifyLabel zeroProbLabel) tagMap


-- | Schematized dataset.
schemed
  :: (X.Word w, Ord t, Ord s)
  => (t -> s)
  -> Schema w s a
  -> [X.Sent w t]
  -> [CRF.SentL Ob s]
schemed simplify schema =
    map onSent
  where
    onSent dag0 =
        let dag = X.mapSent simplify dag0
            mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        in  fmap (uncurry CRF.mkWordL) $
            DAG.zipE (schematize schema dag) (fmap mkProb dag)


-- | Retrieve the tagset in the given sentence.
tagSetIn :: (Ord t) => X.Sent w t -> S.Set t
tagSetIn dag = S.fromList
  [ tag
  | edgeID <- DAG.dagEdges dag
  , let edge = DAG.edgeLabel edgeID dag
  , tag <- M.keys . X.unWMap . X.tags $ edge ]
