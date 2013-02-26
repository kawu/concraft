{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Disamb
(
-- * Types
  Disamb (..)

-- * Disambiguation
, disamb
, disambSent
, disambDoc

-- * Training
, TrainConf (..)
, train
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Foldable (Foldable, foldMap)
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Control.Monad.Ox as Ox
import qualified Data.CRF.Chain2.Generic.External as CRF

import NLP.Concraft.Schema hiding (schematize)
import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F

import qualified NLP.Concraft.Disamb.Tiered as Tier
import qualified NLP.Concraft.Disamb.Positional as P
import qualified Data.Tagset.Positional as TP
import qualified Numeric.SGD as SGD

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Schema t a -> Mx.Sent t -> CRF.Sent Ob t
schematize schema sent =
    [ CRF.mkWord (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs i = Mx.interpsSet w
        where w = v V.! i

-- | A disambiguation model.
data Disamb = Disamb
    { tagset        :: TP.Tagset
    , tiers         :: [P.Tier]
    , schemaConf    :: SchemaConf
    , crf           :: Tier.CRF Ob P.Part }

instance Binary Disamb where
    put Disamb{..} = put tagset >> put tiers >> put schemaConf >> put crf
    get = Disamb <$> get <*> get <*> get <*> get

-- | Unsplit the complex tag (assuming, that it is one
-- of the interpretations of the word).
unSplit :: Eq t => (r -> t) -> Mx.Word r -> t -> r
unSplit split' word x = fromJust $ find ((==x) . split') (Mx.interps word)

-- -- | CRF training function.
-- type TrainCRF o t c
--     =  IO [CRF.SentL o t]           -- ^ Training data 'IO' action
--     -> Maybe (IO [CRF.SentL o t])   -- ^ Maybe evalation data
--     -> IO c                         -- ^ Resulting model
-- 
-- -- | CRF tagging function.
-- type TagCRF o t = CRF.Sent o t -> [t]

-- | Perform context-sensitive disambiguation.
disamb :: Disamb -> Mx.Sent F.Tag -> [F.Tag]
disamb Disamb{..} sent
    = map (uncurry embed)
    . zip sent
    . Tier.tag crf
    . schematize schema
    . Mx.mapSent split
    $ sent
  where
    schema  = fromConf schemaConf
    split   = P.split tiers . TP.parseTag tagset
    embed   = unSplit split

-- | Tag the sentence.
disambSent :: F.Sent s w -> Disamb -> s -> s
disambSent F.Sent{..} dmb sent =
  flip mergeSent sent
    [ select wMap orig
    | (wMap, orig) <- zip
        (doDmb sent)
        (parseSent sent) ]
  where
    F.Word{..} = wordHandler
    doDmb orig =
        let xs = map extract (parseSent orig)
        in  map (uncurry mkChoice) (zip xs (disamb dmb xs))
    mkChoice word x = Mx.mkWMap
        [ if x == y
            then (x, 1)
            else (y, 0)
        | y <- Mx.interps word ]

-- | Disambiguate document.
disambDoc :: Functor f => F.Doc f s w -> Disamb -> L.Text -> L.Text
disambDoc F.Doc{..} dmb =
    let onSent = disambSent sentHandler dmb
    in  showDoc . fmap onSent . parseDoc

-- | Training configuration.
data TrainConf = TrainConf
    { tagsetT       :: TP.Tagset
    , tiersT        :: [P.Tier]
    , schemaConfT   :: SchemaConf
    , featSelT      :: Tier.FeatSel Tier.Ob [Tier.Lb] Tier.Feat
    , sgdArgsT      :: SGD.SgdArgs }

-- | Train disamb model.
train
    :: Foldable f
    => F.Doc f s w      -- ^ Document format handler
    -> TrainConf        -- ^ Training configuration
    -> FilePath         -- ^ Training file
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO Disamb        -- ^ Resultant model
train format TrainConf{..} trainPath evalPath'Maybe = do
    crf <- Tier.train
        (length tiersT)
        featSelT
        sgdArgsT
        (schemed format schema split trainPath)
        (schemed format schema split <$> evalPath'Maybe)
    return $ Disamb tagsetT tiersT schemaConfT crf
  where
    schema = fromConf schemaConfT
    split  = P.split tiersT . TP.parseTag tagsetT

-- | Schematized data from the plain file.
schemed
    :: (Foldable f, Ord t)
    => F.Doc f s w -> Schema t a -> (F.Tag -> t)
    -> FilePath -> IO [CRF.SentL Ob t]
schemed F.Doc{..} schema split path =
    foldMap onSent . parseDoc <$> L.readFile path
  where
    F.Sent{..} = sentHandler
    F.Word{..} = wordHandler
    onSent sent =
        [zip (schematize schema xs) (map mkDist xs)]
      where
        xs  = map (Mx.mapWord split . extract) (parseSent sent)
        mkDist = CRF.mkDist . M.toList . Mx.unWMap . Mx.tagWMap
