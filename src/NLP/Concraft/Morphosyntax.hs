{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types and functions related to the morphosyntax data layer.

module NLP.Concraft.Morphosyntax
( 
-- * Segment
  Seg (..)
, mapSeg
, interpsSet
, interps

-- * Word classes
, HasOrth (..)
, HasOOV (..)

-- * Sentence
, Sent
, mapSent

-- -- * Conversion
-- , segTag
-- , segText
-- , sentTag
-- , sentText

-- * Weighted collection
, WMap (unWMap)
, mapWMap
, mkWMap
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

--------------------------
-- Segment
--------------------------

-- | A segment parametrized over a word type and a tag type.
data Seg w t = Seg {
    -- | A word represented by the segment.
      word  :: w
    -- | A set of interpretations.  To each interpretation
    -- a weight of appropriateness within the context
    -- is assigned.
    , tags  :: WMap t }
    deriving (Show)

instance (Binary w, Ord t, Binary t) => Binary (Seg w t) where
    put Seg{..} = do
        put word
        put tags
    get = Seg <$> get <*> get

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

-- | Orthographic form.
class HasOrth a where
    orth :: a -> T.Text 

instance HasOrth w => HasOrth (Seg w t) where
    orth = orth . word
    {-# INLINE orth #-}
    
-- | Out-of-vocabulary (OOV) word.
class HasOOV a where
    oov :: a -> Bool

instance HasOOV w => HasOOV (Seg w t) where
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

-- -- | Restore original, textual representation of a sentence.
-- restore :: Sent t -> T.Text
-- restore =
--     let toStr Seg{..} = unSpace space `T.append` orth
--     in  T.concat . map toStr

----------------------
-- Weighted collection
----------------------

-- | A set with a non-negative weight assigned to each of
-- its elements.
newtype WMap a = WMap { unWMap :: M.Map a Double }
    deriving (Show, Eq, Ord, Binary)

-- | Make a weighted collection.  Negative elements will be ignored.
mkWMap :: Ord a => [(a, Double)] -> WMap a
mkWMap = WMap . M.fromListWith (+) . filter ((>=0).snd)

-- | Map function over weighted collection elements. 
mapWMap :: Ord b => (a -> b) -> WMap a -> WMap b
mapWMap f = mkWMap . map (first f) . M.toList . unWMap

-- --------------------------
-- -- Conversion
-- --------------------------
-- 
-- -- | Parse segment tags.
-- segTag :: P.Tagset -> Seg T.Text -> Seg P.Tag
-- segTag tagset = mapSeg (P.parseTag tagset)
-- 
-- -- | Show segment tags.
-- segText :: P.Tagset -> Seg P.Tag -> Seg T.Text
-- segText tagset = mapSeg (P.showTag tagset)
-- 
-- -- | Parse sentence tags.
-- sentTag :: P.Tagset -> Sent T.Text -> Sent P.Tag
-- sentTag = map . segTag
-- 
-- -- | Show sentence tags.
-- sentText :: P.Tagset -> Sent P.Tag -> Sent T.Text
-- sentText = map . segText
