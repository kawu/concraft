{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Disamb
(
-- * Model
  Disamb (..)
, Tier.CRF () 

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
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Data.CRF.Chain2.Generic.External as CRF

import NLP.Concraft.Schema hiding (schematize)
import qualified NLP.Concraft.Morphosyntax as X

import qualified NLP.Concraft.Disamb.Tiered as Tier
import qualified NLP.Concraft.Disamb.Positional as P
import qualified Data.Tagset.Positional as T
import qualified Numeric.SGD as SGD

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Schema w t a -> X.Sent w t -> CRF.Sent Ob t
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
    , crf           :: Tier.CRF Ob P.Atom }

instance Binary Disamb where
    put Disamb{..} = put tiers >> put schemaConf >> put crf
    get = Disamb <$> get <*> get <*> get

-- | Unsplit the complex tag (assuming, that it is one
-- of the interpretations of the word).
unSplit :: Eq t => (r -> t) -> X.Seg w r -> t -> r
unSplit split' word x = fromJust $ find ((==x) . split') (X.interps word)

-- | Perform context-sensitive disambiguation.
disamb :: (X.HasOOV w, X.HasOrth w) => Disamb -> X.Sent w T.Tag -> [T.Tag]
disamb Disamb{..} sent
    = map (uncurry embed)
    . zip sent
    . Tier.tag crf
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
disambSent :: (X.HasOOV w, X.HasOrth w) => Disamb
           -> X.Sent w T.Tag -> X.Sent w T.Tag
disambSent = include . disamb

-- | Training configuration.
data TrainConf = TrainConf
    { tiersT        :: [P.Tier]
    , schemaConfT   :: SchemaConf
    , sgdArgsT      :: SGD.SgdArgs }

-- | Train disamb model.
train
    :: (X.HasOOV w, X.HasOrth w)
    => TrainConf                        -- ^ Training configuration
    -> [X.Sent w T.Tag]                 -- ^ Training data
    -> Maybe [X.Sent w T.Tag]           -- ^ Maybe evaluation data
    -> IO Disamb                        -- ^ Resultant model
train TrainConf{..} trainData evalData'Maybe = do
    crf <- Tier.train
        (length tiersT)
        sgdArgsT
        (retSchemed schema split trainData)
        (retSchemed schema split <$> evalData'Maybe)
    return $ Disamb tiersT schemaConfT crf
  where
    retSchemed sc sp = return . schemed sc sp 
    schema = fromConf schemaConfT
    split  = P.split tiersT

-- | Schematized data from the plain file.
schemed :: Ord t => Schema w t a -> (T.Tag -> t)
        -> [X.Sent w T.Tag] -> [CRF.SentL Ob t]
schemed schema split =
    map onSent
  where
    onSent sent =
        let xs  = map (X.mapSeg split) sent
            mkDist = CRF.mkDist . M.toList . X.unWMap . X.tags
        in  zip (schematize schema xs) (map mkDist xs)
