{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NLP.Concraft.Disamb
( Split
, TrainCRF
, TagCRF
, disamb
, disambSent
, disambDoc
, trainOn
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Foldable (Foldable, foldMap)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Control.Monad.Ox as Ox
import qualified Data.CRF.Chain2.Generic.External as CRF

import NLP.Concraft.Schema
import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F

-- | Schematize the input sentence with according to 'schema' rules.
schematize :: Schema t a -> Mx.Sent t -> CRF.Sent Ob t
schematize schema sent =
    [ CRF.mkWord (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs i = Mx.interpsSet w
        where w = v V.! i

-- | Split is just a function from an original tag form
-- to a complex tag form.
type Split r t = r -> t

-- | Unsplit the complex tag (assuming, that it is one
-- of the interpretations of the word).
unSplit :: Eq t => Split r t -> Mx.Word r -> t -> r
unSplit split' word x = fromJust $ find ((==x) . split') (Mx.interps word)

-- | CRF training function.
type TrainCRF o t c
    =  IO [CRF.SentL o t]           -- ^ Training data 'IO' action
    -> Maybe (IO [CRF.SentL o t])   -- ^ Maybe evalation data
    -> IO c                         -- ^ Resulting model

-- | CRF tagging function.
type TagCRF o t = CRF.Sent o t -> [t]

-- | Perform context-sensitive disambiguation.
disamb
    :: (Ord r, Ord t)
    => Schema t a
    -> Split r t
    -> TagCRF Ob t
    -> Mx.Sent r
    -> [r]
disamb schema split tag sent
    = map (uncurry embed)
    . zip sent
    . tag
    . schematize schema
    . Mx.mapSent split
    $ sent
  where
    embed = unSplit split

-- | Tag the sentence.
disambSent
    :: Ord t
    => F.Sent s w
    -> Schema t a
    -> Split F.Tag t
    -> TagCRF Ob t
    -> s -> s
disambSent F.Sent{..} schema split tag sent =
  flip mergeSent sent
    [ select wMap orig
    | (wMap, orig) <- zip
        (doDmb sent)
        (parseSent sent) ]
  where
    F.Word{..} = wordHandler
    doDmb orig =
        let xs = map extract (parseSent orig)
        in  map (uncurry mkChoice) (zip xs (disamb schema split tag xs))
    mkChoice word x = Mx.mkWMap
        [ if x == y
            then (x, 1)
            else (y, 0)
        | y <- Mx.interps word ]

-- | Disambiguate document.
disambDoc
    :: (Functor f, Ord t)
    => F.Doc f s w      -- ^ Document format handler
    -> Schema t a       -- ^ Observation schema
    -> Split F.Tag t    -- ^ Tiered tagging
    -> TagCRF Ob t      -- ^ CRF tagging function
    -> L.Text           -- ^ Input
    -> L.Text           -- ^ Output
disambDoc F.Doc{..} schema split tag =
    let onSent = disambSent sentHandler schema split tag
    in  showDoc . fmap onSent . parseDoc

-- | Train disamb model.
trainOn
    :: (Foldable f, Ord t)
    => F.Doc f s w      -- ^ Document format handler
    -> Schema t a       -- ^ Observation schema
    -> Split F.Tag t    -- ^ Tiered tagging
    -> TrainCRF Ob t c  -- ^ CRF training function
    -> FilePath         -- ^ Training file
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO c             -- ^ Resultant model data
trainOn format schema split train trainPath evalPath'Maybe = do
    crf <- train
        (schemed format schema split trainPath)
        (schemed format schema split <$> evalPath'Maybe)
    return crf

-- | Schematized data from the plain file.
schemed
    :: (Foldable f, Ord t)
    => F.Doc f s w -> Schema t a -> Split F.Tag t
    -> FilePath -> IO [CRF.SentL Ob t]
schemed F.Doc{..} schema split path =
    foldMap onSent . parseDoc <$> L.readFile path
  where
    F.Sent{..} = sentHandler
    F.Word{..} = wordHandler
    onSent sent =
        [zip (schematize schema xs) (map mkDist xs)]
      where
        xs  = map (Mx.mapWord split . extract) (parseSent sent)
        mkDist = CRF.mkDist . M.toList . Mx.unWMap . Mx.tagWMap
