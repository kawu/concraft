-- | The module provides several abstractions for representing external
-- data formats.  Concraft will be able to work with any format which
-- implements those abstractions.

module NLP.Concraft.Format
( Tag
, Word (..)
, Sent (..)
, Format (..)
) where

import Prelude hiding (words, unwords)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified NLP.Concraft.Morphosyntax as M

-- | Textual representation of morphposyntactic tag.
type Tag = T.Text

-- | Words handler.
data Word w = Word {
    -- | Extract information relevant for tagging.
      extract       :: w -> M.Word Tag
    -- | Select the set of morphosyntactic interpretations.
    , select        :: M.Prob Tag -> w -> w }

-- | Sentence handler.
data Sent s w = Sent {
    -- | Split sentence into a list of words.
      split         :: s -> [w]
    -- | Merge words with a sentence.
    , unSplit       :: [w] -> s -> s
    -- | Words handler.
    , wordHandler   :: Word w }

-- | Document format.
data Format f s w = Format {
    -- | Parse textual interpretations into a functor with
    -- sentence elements.
      parse         :: L.Text -> f s
    -- | Show textual reprezentation of a document.
    , unParse       :: f s -> L.Text
    -- | Sentence handler.
    , sentHandler   :: Sent s w }
