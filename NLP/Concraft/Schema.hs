{-# LANGUAGE OverloadedStrings #-}

module NLP.Concraft.Schema
( Schema
, Ox
, Ob
, guessSchemaDefault
, disambSchemaDefault
) where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox

import qualified NLP.Concraft.Morphosyntax as Mx

-- | The Ox monad specialized to word token type and text observations.
type Ox t a = Ox.Ox (Mx.Word t) T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
type Schema t a = V.Vector (Mx.Word t) -> Int -> Ox t a

-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)

-- | Default guessing schema.
guessSchemaDefault :: Schema t ()
guessSchemaDefault sent = \k -> do
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

-- | Default disambiguation schema.
disambSchemaDefault :: Schema t ()
disambSchemaDefault sent = \k -> do
    mapM_ (Ox.save . lowOrth) [k - 1, k, k + 1]
    _ <- Ox.whenJT (Mx.oov `at` k) $ do
        mapM_ (Ox.save . lowPref k) [1, 2, 3]
        mapM_ (Ox.save . lowSuff k) [1, 2, 3]
        Ox.save (isBeg k <> pure "-" <> shapeP k)
    return ()
  where
    at          = Ox.atWith sent
    lowOrth i   = T.toLower <$> Mx.orth `at` i
    lowPref i j = Ox.prefix j =<< lowOrth i
    lowSuff i j = Ox.suffix j =<< lowOrth i
    shape i     = Ox.shape <$> Mx.orth `at` i
    shapeP i    = Ox.pack <$> shape i
    isBeg i     = (Just . boolF) (i == 0)
    boolF True  = "T"
    boolF False = "F"
    x <> y      = T.append <$> x <*> y
