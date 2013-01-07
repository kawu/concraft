{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module NLP.Concraft.Disamb.Tiered
( Ob (..)
, Lb (..)
, Feat (..)
, featGen
, CodecData
, codec
) where

import Control.Applicative ((<$>))
import Control.Comonad.Trans.Store (store)
import Data.Ix (Ix)
import Data.Maybe (fromJust)
import Data.List (zip4)
import Data.Lens.Common (Lens(..))
import Data.Binary (Binary)
-- import Data.Binary (Binary, get, put, Put, Get)
import qualified Data.Map as M
import qualified Data.Vector as V

import Data.CRF.Chain2.Generic.Model (FeatGen(..))
-- import Data.CRF.Chain2.Generic.Internal
-- import Data.CRF.Chain2.Generic.External
import qualified Control.Monad.Codec as C
import Data.CRF.Chain2.Generic.Codec (Codec(..))

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
