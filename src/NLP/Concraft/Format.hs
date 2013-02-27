-- | The module provides several abstractions for representing external
-- data formats.  Concraft will be able to work with any format which
-- implements those abstractions.

module NLP.Concraft.Format
( Tag
, Word (..)
, Sent (..)
, Doc (..)
) where

import Prelude hiding (words, unwords)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified NLP.Concraft.Morphosyntax as M

-- | Textual representation of morphposyntactic tag.
type Tag = T.Text

-- | Word handler.
data Word w = Word {
    -- | Extract information relevant for tagging.
      extract       :: w -> M.Word Tag
    -- | Select the set of morphosyntactic interpretations.
    , select        :: M.WMap Tag -> w -> w }

-- | Sentence handler.
data Sent s w = Sent {
    -- | Split sentence into a list of words.
      parseSent     :: s -> [w]
    -- | Merge words with a sentence.
    , mergeSent     :: [w] -> s -> s
    -- | Words handler.
    , wordHandler   :: Word w }

-- | Document format.
data Doc f s w = Doc {
    -- | Parse textual interpretations into a functor with
    -- sentence elements.
      parseDoc      :: L.Text -> f s
    -- | Show textual reprezentation of a document.
    , showDoc       :: f s -> L.Text
    -- | Sentence handler.
    , sentHandler   :: Sent s w }
