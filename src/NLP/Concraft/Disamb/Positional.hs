{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The module provides functions for splitting positional tags.
-- They can be used together with the layered disambiguation model.

module NLP.Concraft.Disamb.Positional
( Tier (..)
, Atom (..)
, select
, split
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
    -- | End-of-sentence marker.
    , withEos   :: Bool
    -- | Tier grammatical attributes.
    , withAtts  :: S.Set TP.Attr
    }

instance Binary Tier where
    put Tier{..} = put withPos >> put withEos >> put withAtts
    get = Tier <$> get <*> get <*> get

-- | An atomic part of morphosyntactic tag with optional POS.
data Atom = Atom
    { pos   :: Maybe TP.POS
    , atts  :: M.Map TP.Attr T.Text
    , eos   :: Maybe Bool
      -- ^ NOTE: could be simplified to Bool, but this way it's more readable
    } deriving (Show, Eq, Ord)

instance Binary Atom where
    put Atom{..} = put pos >> put atts >> put eos
    get = Atom <$> get <*> get <*> get

-- | Select tier attributes.
select
  :: Tier       -- ^ The tier
  -> TP.Tag     -- ^ The positional tag
  -> Maybe Bool -- ^ (Maybe) end-of-sentence marker
  -> Atom
select Tier{..} tag eos = Atom
    { pos  = if withPos then Just (TP.pos tag) else Nothing
    , atts = M.filterWithKey (\k _ -> k `S.member` withAtts) (TP.atts tag)
    , eos  = if withEos then eos else Nothing
    }

-- | Split the positional tag.
split
  :: [Tier]     -- ^ The tiers
  -> TP.Tag     -- ^ The positional tag
  -> Maybe Bool -- ^ (Maybe) end-of-sentence marker
  -> [Atom]
split tiers tag eos =
    [ select tier tag eos
    | tier <- tiers ]
