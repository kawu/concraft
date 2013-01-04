{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NLP.Concraft.Disamb
( Ox
, Schema
, Ob
, schema
, schematize
, Split
, ChainCRF (..)
, Disamb (..)
, disamb
, disambDoc
, trainOn
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Foldable (Foldable, foldMap)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Data.CRF.Chain2.Generic.External as CRF
import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox

import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F

-- | Interpretations of the word.
interpsSet :: Mx.Word t -> S.Set t
interpsSet = M.keysSet . Mx.unProb . Mx.tagProb

-- | Interpretations of the word.
interps :: Mx.Word t -> [t]
interps = S.toList . interpsSet

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

-- | Schematize the input sentence according to 'schema' rules.
schematize :: Mx.Sent t -> CRF.Sent Ob t
schematize sent =
    [ CRF.mkWord (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs = interpsSet . (v V.!)

-- | Split is just a function from an original tag form
-- to a complex tag form.
type Split r t = r -> t

-- | Unsplit the complex tag (assuming, that it is one
-- of the interpretations of the word).
unSplit :: Eq t => Split r t -> Mx.Word r -> t -> r
unSplit split' word x = fromJust $ find ((==x) . split') (interps word)

-- | Chain CRF abstraction.
data ChainCRF c t = ChainCRF
    { tag :: c -> CRF.Sent Ob t -> [t]
    , train
        :: IO [CRF.SentL Ob t]          -- ^ Training data 'IO' action
        -> Maybe (IO [CRF.SentL Ob t])  -- ^ Maybe evalation data
        -> IO c }                       -- ^ Resulting model

-- | The disambiguation model.
-- c : CRF model
-- r : raw tag form
-- t : processed tag (e.g. layered)
data Disamb c r t = Disamb
    { chain :: ChainCRF c t
    , split :: Split r t
    , crf   :: c }

-- | Perform context-sensitive disambiguation.
disamb :: (Ord r, Ord t) => Disamb c r t -> Mx.Sent r -> [r]
disamb Disamb{..} sent
    = map (uncurry embed)
    . zip sent
    . tag chain crf
    . schematize 
    . Mx.mapSent split
    $ sent
  where
    embed = unSplit split

-- | Tag the sentence.
disambSent
    :: Ord t
    => F.Sent s w
    -> Disamb c F.Tag t
    -> s -> s
disambSent F.Sent{..} dmb sent =
  flip mergeSent sent
    [ select prob orig
    | (prob, orig) <- zip
        (doDmb sent)
        (parseSent sent) ]
  where
    F.Word{..} = wordHandler
    doDmb orig =
        let xs = map extract (parseSent orig)
        in  map (uncurry mkChoice) (zip xs (disamb dmb xs))
    mkChoice word x = Mx.mkProb
        [ if x == y
            then (x, 1)
            else (x, 0)
        | y <- interps word ]

-- | Disamb file.
disambDoc
    :: (Functor f, Ord t)
    => F.Doc f s w      -- ^ Document format handler
    -> Disamb c F.Tag t -- ^ Disambiguation model
    -> L.Text           -- ^ Input
    -> L.Text           -- ^ Output
disambDoc F.Doc{..} dmb =
    let onSent = disambSent sentHandler dmb
    in  showDoc . fmap onSent . parseDoc

-- | Train disamb model.
trainOn
    :: (Foldable f, Ord t)
    => F.Doc f s w
    -> ChainCRF c t
    -> Split F.Tag t    -- ^ Tiered tagging
    -> FilePath         -- ^ Training file
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO (Disamb c F.Tag t)
trainOn format chain'@ChainCRF{..} split' trainPath evalPath'Maybe = do
    crf' <- train
        (schemed format split' trainPath)
        (schemed format split' <$> evalPath'Maybe)
    return $ Disamb chain' split' crf'

-- | Schematized data from the plain file.
schemed
    :: (Foldable f, Ord t)
    => F.Doc f s w -> Split F.Tag t
    -> FilePath -> IO [CRF.SentL Ob t]
schemed F.Doc{..} split path =
    foldMap onSent . parseDoc <$> L.readFile path
  where
    F.Sent{..} = sentHandler
    F.Word{..} = wordHandler
    onSent sent =
        [zip (schematize xs) (map mkDist xs)]
      where
        xs  = map (Mx.mapWord split . extract) (parseSent sent)
        mkDist = CRF.mkDist . M.toList . Mx.unProb . Mx.tagProb
