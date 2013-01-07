{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The module provides functions for splitting positional tags.
-- They can be used together with the layered disambiguation model.

module NLP.Concraft.Disamb.Positional
( Tier (..)
, Part (..)
, select
, split
, tierConfDefault
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tagset.Positional as TP

-- | A tier description.
data Tier = Tier {
    -- | Does it include the part of speech?
      withPos   :: Bool
    -- | Tier grammatical attributes.
    , withAtts  :: S.Set TP.Attr }

instance Binary Tier where
    put Tier{..} = put withPos >> put withAtts
    get = Tier <$> get <*> get

-- | An atomic part of morphosyntactic tag with optional POS.
data Part = Part
    { pos   :: Maybe TP.POS
    , atts  :: M.Map TP.Attr T.Text }
    deriving (Show, Eq, Ord)

instance Binary Part where
    put Part{..} = put pos >> put atts
    get = Part <$> get <*> get

-- | Select tier attributes.
select :: Tier -> TP.Tag -> Part
select Tier{..} tag = Part
    { pos   = if withPos then Just (TP.pos tag) else Nothing
    , atts  = M.filterWithKey (\k _ -> k `S.member` withAtts) (TP.atts tag) }

-- | Split the positional tag.
split :: [Tier] -> TP.Tag -> [Part]
split tiers tag =
    [ select tier tag
    | tier <- tiers ]

-- | Default tiered tagging configuration.
tierConfDefault :: [Tier]
tierConfDefault =
    [tier1, tier2]
  where
    tier1 = Tier True $ S.fromList ["cas", "per"]
    tier2 = Tier False $ S.fromList
        [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
        , "acn", "ppr", "agg", "vlc", "dot" ]
