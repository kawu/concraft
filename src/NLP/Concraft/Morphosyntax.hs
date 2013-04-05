{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Types and functions related to the morphosyntax data layer.


module NLP.Concraft.Morphosyntax
( 
-- * Segment
  Seg (..)
, mapSeg
, interpsSet
, interps

-- * Word classes
, Word (..)

-- * Sentence
, Sent
, mapSent
, SentO (..)
, mapSentO

-- * Weighted collection
, WMap (unWMap)
, mapWMap
, mkWMap
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Data.Aeson
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L


--------------------------
-- Segment
--------------------------


-- | A segment parametrized over a word type and a tag type.
data Seg w t = Seg {
    -- | A word represented by the segment.  Typically it will be
    -- an instance of the `Word` class.
      word  :: w
    -- | A set of interpretations.  To each interpretation
    -- a weight of appropriateness within the context
    -- is assigned.
    , tags  :: WMap t }
    deriving (Show)


instance ToJSON w => ToJSON (Seg w T.Text) where
    toJSON Seg{..} = object
        [ "word" .= word
        , "tags" .= unWMap tags ]

instance FromJSON w => FromJSON (Seg w T.Text) where
    parseJSON (Object v) = Seg
        <$> v .: "word"
        <*> (WMap <$> v .: "tags")
    parseJSON _ = error "parseJSON (segment): absurd"


-- | Map function over segment tags.
mapSeg :: Ord b => (a -> b) -> Seg w a -> Seg w b
mapSeg f w = w { tags = mapWMap f (tags w) }


-- | Interpretations of the segment.
interpsSet :: Seg w t -> S.Set t
interpsSet = M.keysSet . unWMap . tags


-- | Interpretations of the segment.
interps :: Seg w t -> [t]
interps = S.toList . interpsSet


--------------------------
-- Word classes
--------------------------


class Word a where
    -- | Orthographic form.
    orth    :: a -> T.Text 
    -- | Out-of-vocabulary (OOV) word.
    oov     :: a -> Bool


instance Word w => Word (Seg w t) where
    orth = orth . word
    {-# INLINE orth #-}
    oov = oov . word
    {-# INLINE oov #-}


----------------------
-- Sentence
----------------------


-- | A sentence.
type Sent w t = [Seg w t]

-- | Map function over sentence tags.
mapSent :: Ord b => (a -> b) -> Sent w a -> Sent w b
mapSent = map . mapSeg

-- | A sentence with original, textual representation.
data SentO w t = SentO
    { segs  :: Sent w t
    , orig  :: L.Text }
    deriving (Show)

-- | Map function over sentence tags.
mapSentO :: Ord b => (a -> b) -> SentO w a -> SentO w b
mapSentO f x =
    let segs' = mapSent f (segs x)
    in  x { segs = segs' }

----------------------
-- Weighted collection
----------------------

-- | A set with a non-negative weight assigned to each of
-- its elements.
newtype WMap a = WMap { unWMap :: M.Map a Double }
    deriving (Show, Eq, Ord)


-- | Make a weighted collection.  Negative elements will be ignored.
mkWMap :: Ord a => [(a, Double)] -> WMap a
mkWMap = WMap . M.fromListWith (+) . filter ((>=0).snd)


-- | Map function over weighted collection elements. 
mapWMap :: Ord b => (a -> b) -> WMap a -> WMap b
mapWMap f = mkWMap . map (first f) . M.toList . unWMap
