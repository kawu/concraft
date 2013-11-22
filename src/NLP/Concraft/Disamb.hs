{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft.Disamb
(
-- * Model
  Disamb (..)

-- * Tiers
, P.Tier (..)
, P.Atom (..)

-- * Disambiguation
, disamb
, include
, disambSent

-- * Training
, TrainConf (..)
, train

-- * Pruning
, prune
) where


import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Data.CRF.Chain2.Tiers as CRF

import NLP.Concraft.Schema hiding (schematize)
import qualified NLP.Concraft.Morphosyntax as X

import qualified NLP.Concraft.Disamb.Positional as P
import qualified Data.Tagset.Positional as T
import qualified Numeric.SGD as SGD


-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Schema w [t] a -> X.Sent w [t] -> CRF.Sent Ob t
schematize schema sent =
    [ CRF.mkWord (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs i = X.interpsSet w
        where w = v V.! i


-- | A disambiguation model.
data Disamb = Disamb
    { tiers         :: [P.Tier]
    , schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob P.Atom }


instance Binary Disamb where
    put Disamb{..} = put tiers >> put schemaConf >> put crf
    get = Disamb <$> get <*> get <*> get


-- | Unsplit the complex tag (assuming, that it is one
-- of the interpretations of the word).
unSplit :: Eq t => (r -> t) -> X.Seg w r -> t -> r
unSplit split' word x = fromJust $ find ((==x) . split') (X.interps word)


-- | Perform context-sensitive disambiguation.
disamb :: X.Word w => Disamb -> X.Sent w T.Tag -> [T.Tag]
disamb Disamb{..} sent
    = map (uncurry embed)
    . zip sent
    . CRF.tag crf
    . schematize schema
    . X.mapSent split
    $ sent
  where
    schema  = fromConf schemaConf
    split   = P.split tiers
    embed   = unSplit split


-- | Insert disambiguation results into the sentence.
include :: (X.Sent w T.Tag -> [T.Tag]) -> X.Sent w T.Tag -> X.Sent w T.Tag
include f sent =
    [ word { X.tags = tags }
    | (word, tags) <- zip sent sentTags ]
  where
    sentTags = map (uncurry select) (zip (f sent) sent)
    select x word = X.mkWMap
        [ (y, if x == y then 1 else 0)
        | y <- X.interps word ]


-- | Combine `disamb` with `include`. 
disambSent :: X.Word w => Disamb -> X.Sent w T.Tag -> X.Sent w T.Tag
disambSent = include . disamb


-- | Prune disamb model: discard model features with absolute values
-- (in log-domain) lower than the given threshold.
prune :: Double -> Disamb -> Disamb
prune x dmb =
    let crf' = CRF.prune x (crf dmb)
    in  dmb { crf = crf' }


-- | Training configuration.
data TrainConf
    = TrainConf
        { tiersT        :: [P.Tier]
        , schemaConfT   :: SchemaConf
        , sgdArgsT      :: SGD.SgdArgs
        , onDiskT       :: Bool }
    | ReTrainConf
        { initDmb       :: Disamb
        , sgdArgsT      :: SGD.SgdArgs
        , onDiskT       :: Bool }


-- | Train disamb model.
train
    :: X.Word w
    => TrainConf                -- ^ Training configuration
    -> IO [X.Sent w T.Tag]      -- ^ Training data
    -> IO [X.Sent w T.Tag]      -- ^ Evaluation data
    -> IO Disamb                -- ^ Resultant model
train TrainConf{..} trainData evalData = do
    crf <- CRF.train (length tiersT) CRF.selectHidden sgdArgsT onDiskT
        (schemed schema split <$> trainData)
        (schemed schema split <$> evalData)
    putStr "\nNumber of features: " >> print (CRF.size crf)
    return $ Disamb tiersT schemaConfT crf
  where
    schema = fromConf schemaConfT
    split  = P.split tiersT

-- Improve disamb model.
train ReTrainConf{..} trainData evalData = do
    crf' <- CRF.reTrain crf sgdArgsT onDiskT
        (schemed schema split <$> trainData)
        (schemed schema split <$> evalData)
    putStr "\nNumber of features: " >> print (CRF.size crf')
    return $ initDmb { crf = crf' }
  where
    Disamb{..} = initDmb
    schema = fromConf schemaConf
    split  = P.split tiers


-- | Schematized data from the plain file.
schemed :: Ord t => Schema w [t] a -> (T.Tag -> [t])
        -> [X.Sent w T.Tag] -> [CRF.SentL Ob t]
schemed schema split =
    map onSent
  where
    onSent sent =
        let xs  = map (X.mapSeg split) sent
            mkProb = CRF.mkProb . M.toList . X.unWMap . X.tags
        in  zip (schematize schema xs) (map mkProb xs)
