{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NLP.Concraft.Disamb
( Ox
, Schema
, Ob
, schema
, schematize
-- , TierConf
-- , tear
-- , deTear
-- , deTears
-- , Disamb (..)
-- , disamb
-- , train
-- , tag
-- 
-- -- * Feature selection
-- , FeatSel
-- , selectPresent
-- , selectHidden
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Foldable (Foldable, foldMap)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox
import qualified Data.CRF.Chain2.Generic.External as CRF
import qualified Numeric.SGD as SGD
import qualified Data.Tagset.Positional as TP

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

-- | The disambiguation model.
-- c : CRF model
-- r : raw tag form
-- t : processed tag (e.g. layered)
data Disamb c r t = Disamb
    -- { crf       :: CRF.CRF Ob Tag Tag
    -- { crf   :: c
    { crf   :: c t
    , tear  :: r -> t }

-- | Unsplit the complex tag (assuming, that it is one
-- of interpretations of the word).
unTear :: Eq t => (r -> t) -> Mx.Word r -> t -> r
unTear tear' word x = fromJust $ find ((==x) . tear') (interps word)

class ChainCRF c t where
    tag :: c t -> CRF.Sent Ob t -> [t]

-- | Perform context-sensitive disambiguation.
disamb
    :: (Ord r, Ord t, ChainCRF c t)
    => Disamb c r t -> Mx.Sent r -> Mx.Sent r
disamb Disamb{..} sent
    = map (uncurry embed)
    . zip sent
    . tag crf
    . schematize 
    . Mx.mapSent tear
    $ sent
  where
    embed word tag =
        let raw = unTear tear word tag
            choice = mkChoice word raw
        in  word { Mx.tagProb = choice }
    mkChoice word x = Mx.mkProb
        [ if x == y
            then (x, 1)
            else (x, 0)
        | y <- interps word ]

-- | Tag the sentence.
disambSent
    :: (Ord t, ChainCRF c t)
    => F.Sent s w
    -> Disamb c F.Tag t
    -> s -> s
disambSent F.Sent{..} dmb sent =
  flip unSplit sent
    [ select (Mx.tagProb word) orig
    | (word, orig) <- zip
        (doDmb sent)
        (split sent) ]
  where
    -- Word handler.
    F.Word{..} = wordHandler
    -- Perform disambiguation.
    doDmb = disamb dmb . map extract . split

-- | Disamb file.
disambFile
    :: (Functor f, Ord t, ChainCRF c t)
    => F.Format f s w   -- ^ Format handler
    -> Disamb c F.Tag t -- ^ Disambiguation model
    -> L.Text           -- ^ Input
    -> L.Text           -- ^ Output
disambFile F.Format{..} dmb =
    let onSent = disambSent sentHandler dmb
    in  unParse . fmap onSent . parse

-- type FeatSel = CRF.FeatSel CRF.Ob CRF.Lb CRF.Feat
-- 
-- -- | Present features selection.
-- selectPresent :: FeatSel
-- selectPresent = CRF.selectPresent
-- 
-- -- | Hidden features selection.
-- selectHidden :: FeatSel
-- selectHidden = CRF.selectHidden
-- 
-- -- | Train disamb model.
-- train
--     :: Foldable f
--     => F.Format f s w
--     -> SGD.SgdArgs      -- ^ SGD parameters 
--     -> FilePath         -- ^ File with positional tagset definition
--     -> T.Text        	-- ^ The tag indicating unknown words
--     -> TierConf         -- ^ Tiered tagging configuration
--     -> FeatSel          -- ^ Feature selection 
--     -> FilePath         -- ^ Training file
--     -> Maybe FilePath   -- ^ Maybe eval file
--     -> IO Disamb
-- train format sgdArgs tagsetPath ign tierConf ftSel
--       trainPath evalPath'Maybe = do
--     _tagset <- TP.parseTagset tagsetPath <$> readFile tagsetPath
--     _crf <- CRF.train sgdArgs ftSel
--         (schemed format _tagset ign tierConf trainPath)
--         (schemed format _tagset ign tierConf <$> evalPath'Maybe)
--     return $ Disamb _crf _tagset tierConf
-- 
-- -- | Schematized data from the plain file.
-- schemed
--     :: Foldable t => F.Format t s w -> TP.Tagset -> T.Text
--     -> TierConf -> FilePath -> IO [CRF.SentL Ob (Tag, Tag)]
-- schemed F.Format{..} tagset _ign cfg path =
--     foldMap onSent . parse <$> L.readFile path
--   where
--     F.Sent{..} = sentHandler
--     F.Word{..} = wordHandler
--     onSent sent =
--         [zip (schematize xs) (map mkDist xs)]
--       where
--         xs  = map (mapWord smash . extract) (split sent)
--         smash = tear cfg . parseTag
--         parseTag = TP.parseTag tagset
--         mkDist = CRF.mkDist . M.toList . unProb . tags
