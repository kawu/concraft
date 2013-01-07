{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Disamb.Tiered
(
-- * Tiered model
  Ob (..)
, Lb (..)
, Feat (..)
, CRF (..)
, train
, tag

-- * Feature selection
, FeatSel
, selectHidden
, selectPresent
) where

import Control.Applicative ((<$>), (<*>))
import Control.Comonad.Trans.Store (store)
import Control.Monad (guard)
import Data.Ix (Ix, inRange, range)
import Data.Maybe (catMaybes, fromJust)
import Data.List (zip4, foldl1')
import Data.Lens.Common (Lens(..))
import Data.Binary (Binary, get, put, Put, Get)
import Data.Vector.Binary ()
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Array.Unboxed as A

import Data.CRF.Chain2.Generic.Codec
    ( Codec(..), mkCodec, encodeDataL
    , encodeSent, decodeLabels, unJust )
import Data.CRF.Chain2.Generic.Model
    ( FeatGen(..), Model, FeatSel
    , selectHidden, selectPresent
    , core, withCore )
import Data.CRF.Chain2.Generic.Internal (FeatIx(..))
import qualified Data.CRF.Chain2.Generic.Inference as I
import qualified Data.CRF.Chain2.Generic.Train as Train
import qualified Data.CRF.Chain2.Generic.FeatMap as F
import qualified Control.Monad.Codec as C
import qualified Numeric.SGD as SGD
import qualified NLP.Concraft.Disamb as D

-- | Observation.
newtype Ob = Ob { unOb :: Int } deriving (Show, Eq, Ord, Ix, Binary)

-- | [Sub]label.
newtype Lb = Lb { unLb :: Int } deriving (Show, Eq, Ord, Ix, Binary)

-- | Feature.
data Feat
    = TFeat3
        { x1    :: {-# UNPACK #-} !Lb
        , x2    :: {-# UNPACK #-} !Lb
        , x3    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    | TFeat2
        { x1    :: {-# UNPACK #-} !Lb
        , x2    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    | TFeat1
        { x1    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    | OFeat
        { ob    :: {-# UNPACK #-} !Ob
        , x1    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    deriving (Show, Eq, Ord)

instance Binary Feat where
    put (OFeat o x k)       = putI 0 >> put o >> put x >> put k
    put (TFeat3 x y z k)    = putI 1 >> put x >> put y >> put z >> put k
    put (TFeat2 x y k)      = putI 2 >> put x >> put y >> put k
    put (TFeat1 x k)        = putI 3 >> put x >> put k
    get = getI >>= \i -> case i of
        0   -> OFeat  <$> get <*> get <*> get
        1   -> TFeat3 <$> get <*> get <*> get <*> get
        2   -> TFeat2 <$> get <*> get <*> get
        3   -> TFeat1 <$> get <*> get
        _   -> error "get feature: unknown code"

putI :: Int -> Put
putI = put
{-# INLINE putI #-}

getI :: Get Int
getI = get
{-# INLINE getI #-}

-- | Feature generation for complex [Lb] label type.
featGen :: FeatGen Ob [Lb] Feat
featGen = FeatGen
    { obFeats   = obFeats'
    , trFeats1  = trFeats1'
    , trFeats2  = trFeats2'
    , trFeats3  = trFeats3' }
  where
    obFeats' ob' xs =
        [ OFeat ob' x k
        | (x, k) <- zip xs [0..] ]
    trFeats1' xs =
        [ TFeat1 x k
        | (x, k) <- zip xs [0..] ]
    trFeats2' xs1 xs2 =
        [ TFeat2 x1' x2' k
        | (x1', x2', k) <-
          zip3 xs1 xs2 [0..] ]
    trFeats3' xs1 xs2 xs3 =
        [ TFeat3 x1' x2' x3' k
        | (x1', x2', x3', k) <-
          zip4 xs1 xs2 xs3 [0..] ]

-- | Codec internal data.  The first component is used to
-- encode observations of type a, the second one is used to
-- encode labels of type [b].
type CodecData a b =
    ( C.AtomCodec a
    , V.Vector (C.AtomCodec (Maybe b)) )

obLens :: Lens (a, b) a
obLens = Lens $ \(a, b) -> store (\a' -> (a', b)) a

lbLens :: Int -> Lens (a, V.Vector b) b
lbLens k = Lens $ \(a, b) -> store
    (\x -> (a, b V.// [(k, x)]))
    (b V.! k)

-- | Codec dependes on the number of layers. 
codec :: (Ord a, Ord b) => Int -> Codec a [b] (CodecData a b) Ob [Lb]
codec n = Codec
    { empty =
        let x = C.execCodec C.empty (C.encode C.idLens Nothing)
        in  (C.empty, V.replicate n x)
    , encodeObU = fmap Ob . C.encode' obLens
    , encodeObN = fmap (fmap Ob) . C.maybeEncode obLens
    , encodeLbU = \ xs -> sequence
        [ Lb <$> C.encode (lbLens k) (Just x)
        | (x, k) <- zip xs [0..] ]
    , encodeLbN = \ xs ->
        let encode lens x = C.maybeEncode lens (Just x) >>= \mx -> case mx of
                Just x' -> return x'
                Nothing -> fromJust <$> C.maybeEncode lens Nothing
        in  sequence
                [ Lb <$> encode (lbLens k) x
                | (x, k) <- zip xs [0..] ]
    , decodeLbC = \ xs -> sequence <$> sequence
        [ C.decode (lbLens k) (unLb x)
        | (x, k) <- zip xs [0..] ]
    , hasLabel = \ cdcData xs -> and
        [ M.member
            (Just x)
            (C.to $ snd cdcData V.! k)
        | (x, k) <- zip xs [0..] ] }

-- | Dummy feature index.
dummy :: FeatIx
dummy = FeatIx (-1)
{-# INLINE dummy #-}

-- | Transition map restricted to a particular tagging layer.
type TransMap = A.UArray (Lb, Lb, Lb) FeatIx

-- | CRF feature map.
data FeatMap a = FeatMap
    { transMaps	:: V.Vector TransMap
    , otherMap 	:: M.Map Feat FeatIx }

instance Binary (FeatMap Feat) where
    put FeatMap{..} = put transMaps >> put otherMap
    get = FeatMap <$> get <*> get

instance F.FeatMap FeatMap Feat where
    featIndex (TFeat3 x y z k) (FeatMap v _) = do
        m  <- v V.!? k
        ix <- m !? (x, y, z)
        guard (ix /= dummy)
        return ix
    featIndex x (FeatMap _ m) = M.lookup x m
    mkFeatMap xs = FeatMap
        ( V.fromList
            [ mkArray . catMaybes $
                map (getTFeat3 k) xs
            | k <- [0 .. maxLayerNum xs] ] )
        (M.fromList (filter (isOther . fst) xs))
      where
        maxLayerNum = maximum . map (ln.fst)
        getTFeat3 i (TFeat3 x y z j, v)
            | i == j                = Just ((x, y, z), v)
            | otherwise             = Nothing
        getTFeat3 _ _               = Nothing
        isOther (TFeat3 _ _ _ _)    = False
        isOther _                   = True
        mkArray ys =
            let p = foldl1' updateMin (map fst ys)
                q = foldl1' updateMax (map fst ys)
                updateMin (x, y, z) (x', y', z') =
                    (min x x', min y y', min z z')
                updateMax (x, y, z) (x', y', z') =
                    (max x x', max y y', max z z')
                zeroed pq = A.array pq [(k, dummy) | k <- range pq]
            in  zeroed (p, q) A.// ys

(!?) :: (Ix i, A.IArray a b) => a i b -> i -> Maybe b
m !? x = if inRange (A.bounds m) x
    then Just (m A.! x)
    else Nothing
{-# INLINE (!?) #-}

-- | CRF model data.
data CRF a b = CRF
    { numOfLayers   :: Int
    , codecData     :: CodecData a b
    , model         :: Model FeatMap Ob [Lb] Feat }

instance (Ord a, Ord b, Binary a, Binary b) => Binary (CRF a b) where
    put CRF{..} = put numOfLayers >> put codecData >> put (core model)
    get = CRF <$> get <*> get <*> do
        _core <- get
        return $ withCore _core featGen

-- | Codec specification given the number of layers.
codecSpec
    :: (Ord a, Ord b) => Int
    -> Train.CodecSpec a [b] (CodecData a b) Ob [Lb]
codecSpec n = Train.CodecSpec
    { Train.mkCodec = mkCodec (codec n)
    , Train.encode  = encodeDataL (codec n) }

-- | Train the CRF using the stochastic gradient descent method.
-- Use the provided feature selection function to determine model
-- features.
train
    :: (Ord o, Ord t)
    => Int                          -- ^ Number of tagging layers
    -> FeatSel Ob [Lb] Feat         -- ^ Feature selection
    -> SGD.SgdArgs                  -- ^ Args for SGD
    -> D.TrainCRF o [t] (CRF o t)
train n featSel sgdArgs trainIO evalIO'Maybe = do
    (_codecData, _model) <- Train.train
        sgdArgs
        (codecSpec n)
        featGen
        featSel
        trainIO
        evalIO'Maybe
    return $ CRF n _codecData _model

-- | Find the most probable label sequence.
tag :: (Ord o, Ord t) => CRF o t -> D.TagCRF o [t]
tag CRF{..} sent
    = onWords . decodeLabels cdc codecData
    . I.tag model . encodeSent cdc codecData
    $ sent
  where
    cdc = codec numOfLayers
    onWords xs =
        [ unJust cdc codecData word x
        | (word, x) <- zip sent xs ]

-- -- | Instantiation of chain CRF.
-- chainCRF
--     :: Ord t
--     => Int                          -- ^ Number of tagging layers
--     -> FeatSel Ob [Lb] Feat         -- ^ Feature selection
--     -> D.ChainCRF (CRF D.Ob t) [t]
-- chainCRF n featSel = D.ChainCRF
--     { D.tag = tag
--     , D.train = train n featSel }
-- 
-- disambConf
--     :: Ord t
--     => Int                          -- ^ Number of tagging layers
--     -> SGD.SgdArgs                  -- ^ Args for SGD
--     -> FeatSel Ob [Lb] Feat         -- ^ Feature selection
--     -> D.Disamb (CRF D.Ob t) r [t]
