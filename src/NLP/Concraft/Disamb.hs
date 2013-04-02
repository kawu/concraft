{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Disamb
(
-- * Model
  Disamb (..)
, Tier.CRF () 

-- * Tiers
, P.Tier (..)
, P.Atom (..)
, P.tiersDefault

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
schematize :: Schema t a -> X.Sent t -> CRF.Sent Ob t
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
unSplit :: Eq t => (r -> t) -> X.Seg r -> t -> r
unSplit split' word x = fromJust $ find ((==x) . split') (X.interps word)

-- | Perform context-sensitive disambiguation.
disamb :: Disamb -> X.Sent T.Tag -> [T.Tag]
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
include :: (X.Sent T.Tag -> [T.Tag]) -> X.Sent T.Tag -> X.Sent T.Tag
include f sent =
    [ word { X.tags = tags }
    | (word, tags) <- zip sent sentTags ]
  where
    sentTags = map (uncurry select) (zip (f sent) sent)
    select x word = X.mkWMap
        [ (y, if x == y then 1 else 0)
        | y <- X.interps word ]

-- | Combine `disamb` with `include`. 
disambSent :: Disamb -> X.Sent T.Tag -> X.Sent T.Tag
disambSent = include . disamb

-- | Training configuration.
data TrainConf = TrainConf
    { tiersT        :: [P.Tier]
    , schemaConfT   :: SchemaConf
    , sgdArgsT      :: SGD.SgdArgs }

-- | Train disamb model.
train
    :: TrainConf                        -- ^ Training configuration
    -> [X.Sent T.Tag]                   -- ^ Training data
    -> Maybe [X.Sent T.Tag]             -- ^ Maybe evaluation data
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
schemed
    :: Ord t => Schema t a -> (T.Tag -> t) -> [X.Sent T.Tag] -> [CRF.SentL Ob t]
schemed schema split =
    map onSent
  where
    onSent sent =
        let xs  = map (X.mapSeg split) sent
            mkDist = CRF.mkDist . M.toList . X.unWMap . X.tags
        in  zip (schematize schema xs) (map mkDist xs)
