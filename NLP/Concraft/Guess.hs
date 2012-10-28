{-# LANGUAGE OverloadedStrings #-}

module NLP.Concraft.Guess
( Ox
, Schema
, Ob
, schema
, schematize
) where

import Control.Applicative (pure, (<$>), (<*>))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox
import qualified Data.CRF.Chain1.Constrained as CRF

import NLP.Concraft.Morphosyntax

-- | The Ox monad specialized to word token type and text observations.
-- TODO: Move to monad-ox package from here and from the nerf library.
type Ox t a = Ox.Ox (Word t) T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
type Schema t a = V.Vector (Word t) -> Int -> Ox t a

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
    lowOrth i   = T.toLower <$> orth `at` i
    lowPref i j = Ox.prefix j =<< lowOrth i
    lowSuff i j = Ox.suffix j =<< lowOrth i
    shape i     = Ox.shape <$> orth `at` i
    shapeP i    = Ox.pack <$> shape i
    knownAt i   = boolF <$> known `at` i
    isBeg i     = (Just . boolF) (i == 0)
    boolF True  = "T"
    boolF False = "F"
    x <> y      = T.append <$> x <*> y

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Ord t => [Word t] -> [CRF.Word Ob t]
schematize sent =
    [ CRF.Word (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs = S.fromList . map tag . S.toList . interps . (v V.!)
