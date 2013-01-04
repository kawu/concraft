{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Concraft.Guess
( Ox
, Schema
, Ob
, schema
, schematize
, Guesser (..)
, guess
, guessDoc
, trainOn
) where

import Prelude hiding (words)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Binary (Binary)
import Data.Foldable (Foldable, foldMap)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox
import qualified Data.CRF.Chain1.Constrained as CRF
import qualified Numeric.SGD as SGD

import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F

-- | The Ox monad specialized to word token type and text observations.
type Ox t a = Ox.Ox (Mx.Word t) T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
-- TODO: Move to monad-ox package from here and from the nerf library.
type Schema t a = V.Vector (Mx.Word t) -> Int -> Ox t a

-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)

schema :: Schema t ()
schema sent = \k -> do
    mapM_ (Ox.save . lowPref k) [1, 2]
    mapM_ (Ox.save . lowSuff k) [1, 2]
    Ox.save (knownAt k)
    Ox.save (isBeg k <> pure "-" <> shapeP k)
  where
    at          = Ox.atWith sent
    lowOrth i   = T.toLower <$> Mx.orth `at` i
    lowPref i j = Ox.prefix j =<< lowOrth i
    lowSuff i j = Ox.suffix j =<< lowOrth i
    shape i     = Ox.shape <$> Mx.orth `at` i
    shapeP i    = Ox.pack <$> shape i
    knownAt i   = boolF <$> (not . Mx.oov) `at` i
    isBeg i     = (Just . boolF) (i == 0)
    boolF True  = "T"
    boolF False = "F"
    x <> y      = T.append <$> x <*> y

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Ord t => Mx.Sent t -> CRF.Sent Ob t
schematize sent =
    [ CRF.Word (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    -- TODO: Handle OOV case in the CRF library, i.e. share sets
    -- of potential interpretations to reduce memory footprint.
    lbs i 
        | Mx.oov w  = S.empty
        | otherwise = M.keysSet . Mx.unProb . Mx.tagProb $ w
        where w = v V.! i

-- | A guesser represented by the conditional random field.
newtype Guesser t = Guesser { crf :: CRF.CRF Ob t }
    deriving (Binary)

-- | Determine the 'k' most probable labels for each word in the sentence.
guess :: Ord t => Int -> Guesser t -> Mx.Sent t -> [[t]]
guess k gsr sent = CRF.tagK k (crf gsr) (schematize sent)

-- | Tag sentence in external format.  Selected interpretations
-- (tags correct within the context) will be preserved.
guessSent :: F.Sent s w -> Int -> Guesser T.Text -> s -> s
guessSent F.Sent{..} k gsr sent = flip mergeSent sent
    [ select pr word
    | (pr, word) <- zip probs (parseSent sent) ]
  where
    -- Extract word handler.
    F.Word{..} = wordHandler
    -- Word in internal format.
    words   = map extract (parseSent sent)
    -- Guessed lists of interpretations for individual words.
    guessed = guess k gsr words
    -- Resultant probability distributions. 
    probs   =
        [ if Mx.oov word
            then addInterps (Mx.tagProb word) xs
            else Mx.tagProb word
        | (xs, word) <- zip guessed words ]
    -- Add new interpretations.
    addInterps pr xs = Mx.mkProb
        $  M.toList (Mx.unProb pr)
        ++ zip xs [0..]

-- | Tag file.
guessDoc
    :: Functor f
    => F.Doc f s w  	-- ^ Document format handler
    -> Int              -- ^ Guesser argument
    -> Guesser T.Text   -- ^ Guesser itself
    -> L.Text           -- ^ Input
    -> L.Text           -- ^ Output
guessDoc F.Doc{..} k gsr
    = showDoc 
    . fmap (guessSent sentHandler k gsr)
    . parseDoc

-- | Train guesser.
trainOn
    :: Foldable f
    => F.Doc f s w
    -> SGD.SgdArgs      -- ^ SGD parameters 
    -> FilePath         -- ^ Training file
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO (Guesser T.Text)
trainOn format sgdArgs trainPath evalPath'Maybe = do
    _crf <- CRF.train sgdArgs
        (schemed format trainPath)
        (schemed format <$> evalPath'Maybe)
        (const CRF.presentFeats)
    return $ Guesser _crf

-- | Schematized data from the plain file.
schemed
    :: Foldable t => F.Doc t s w
    -> FilePath -> IO [CRF.SentL Ob T.Text]
schemed F.Doc{..} path =
    foldMap onSent . parseDoc <$> L.readFile path
  where
    F.Sent{..} = sentHandler
    F.Word{..} = wordHandler
    onSent sent =
        let xs = map extract (parseSent sent)
            mkProb = CRF.mkProb . M.toList . Mx.unProb . Mx.tagProb
        in  [zip (schematize xs) (map mkProb xs)]
