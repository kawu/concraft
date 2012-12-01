{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Disamb
( Tier (..)
, Tag (..)
, select
, splitWord
, splitSent
, Ox
, Schema
, Ob
, schema
, schematize
, TierConf
, tear
, deTear
, deTears
, Disamb (..)
, disamb
, tagFile
, train

-- * Feature selection
, FeatSel
, selectPresent
, selectHidden
) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox
import qualified Data.CRF.Chain2.Pair as CRF
import qualified Numeric.SGD as SGD
import qualified Data.Tagset.Positional as TP

import NLP.Concraft.Morphosyntax
import qualified NLP.Concraft.Plain as P

-- | A tier description.
data Tier = Tier {
    -- | Does it include the part of speech?
      withPos   :: Bool
    -- | Tier grammatical attributes.
    , withAtts  :: S.Set TP.Attr }

instance Binary Tier where
    put Tier{..} = put withPos >> put withAtts
    get = Tier <$> get <*> get

-- | A tag with optional POS.
data Tag = Tag
    { pos   :: Maybe TP.POS
    , atts  :: M.Map TP.Attr T.Text }
    deriving (Show, Eq, Ord)

instance Binary Tag where
    put Tag{..} = put pos >> put atts
    get = Tag <$> get <*> get

-- | Select tier attributes.
select :: Tier -> TP.Tag -> Tag
select Tier{..} tag = Tag
    { pos   = if withPos then Just (TP.pos tag) else Nothing
    , atts  = M.filterWithKey (\k _ -> k `S.member` withAtts) (TP.atts tag) }

-- | The Ox monad specialized to word token type and text observations.
type Ox t a = Ox.Ox (Word t) T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
-- TODO: Move to monad-ox package from here and from the nerf library.
type Schema t a = V.Vector (Word t) -> Int -> Ox t a

-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)

schema :: Schema t ()
schema sent = \k -> do
    mapM_ (Ox.save . lowOrth) [k - 1, k, k + 1]
    _ <- Ox.whenJT (oov `at` k) $ do
        mapM_ (Ox.save . lowPref k) [1, 2, 3]
        mapM_ (Ox.save . lowSuff k) [1, 2, 3]
        Ox.save (isBeg k <> pure "-" <> shapeP k)
    return ()
  where
    at          = Ox.atWith sent
    lowOrth i   = T.toLower <$> orth `at` i
    lowPref i j = Ox.prefix j =<< lowOrth i
    lowSuff i j = Ox.suffix j =<< lowOrth i
    shape i     = Ox.shape <$> orth `at` i
    shapeP i    = Ox.pack <$> shape i
    isBeg i     = (Just . boolF) (i == 0)
    boolF True  = "T"
    boolF False = "F"
    x <> y      = T.append <$> x <*> y

-- | Schematize the input sentence according to 'schema' rules.
schematize :: Sent t -> CRF.Sent Ob t
schematize sent =
    [ CRF.mkWord (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs = tags . (v V.!)

type TierConf = (Tier, Tier)

tear :: TierConf -> TP.Tag -> (Tag, Tag)
tear (t1, t2) = (,) <$> select t1 <*> select t2

-- | Split tags between two layers.
-- TODO: Add support for multiple layers.
splitWord :: TierConf -> Word TP.Tag -> Word (Tag, Tag)
splitWord cfg = mapWord (tear cfg)

splitSent :: TierConf -> Sent TP.Tag -> Sent (Tag, Tag)
splitSent ts = map (splitWord ts)

-- | The disambiguation model.
data Disamb = Disamb
    { crf       :: CRF.CRF Ob Tag Tag
    , tagset    :: TP.Tagset
    , tierConf  :: TierConf }

instance Binary Disamb where
    put Disamb{..} = put crf >> put tagset >> put tierConf
    get = Disamb <$> get <*> get <*> get

-- | Determine the most probable label sequence.
disamb :: Disamb -> Sent TP.Tag -> [TP.Tag]
disamb Disamb{..} sent
    = deTears tierConf sent
    . CRF.tag crf
    . schematize
    . splitSent tierConf
    $ sent

deTears :: TierConf -> Sent TP.Tag -> [(Tag, Tag)] -> [TP.Tag]
deTears cfg sent tiered =
    [ deTear cfg word pair
    | (word, pair) <- zip sent tiered ]

-- | Unsplit the list of tag pairs.  TODO: It can be done without the
-- help of original word.
deTear :: TierConf -> Word TP.Tag -> (Tag, Tag) -> TP.Tag
deTear cfg word tiered =
    fromJust $ find
        ((==tiered) . tear cfg)
        (S.toList $ tags word)

-- | Tag the file.
tagFile
    :: T.Text           -- ^ Tag indicating unknown words
    -> Disamb
    -> FilePath         -- ^ File to tag (plain format)
    -> IO L.Text
tagFile ign dmb path =
    P.showPlain ign . map onSent <$> P.readPlain ign path
  where
    onSent sent =
        [ choose tok y
        | (tok, y) <- zip sent ys ]
      where
        rs = map (fst . P.fromTok) sent
        xs = map (mapWord parseTag) rs
        ys = map showTag (disamb dmb xs)
        choose tok y = P.choose tok (S.singleton y)
        parseTag = TP.parseTag (tagset dmb)
        showTag  = TP.showTag (tagset dmb)

type FeatSel = CRF.FeatSel CRF.Ob CRF.Lb CRF.Feat

-- | Present features selection.
selectPresent :: FeatSel
selectPresent = CRF.selectPresent

-- | Hidden features selection.
selectHidden :: FeatSel
selectHidden = CRF.selectHidden

-- | TODO: Abstract over the format type.
train
    :: SGD.SgdArgs      -- ^ SGD parameters 
    -> FilePath         -- ^ File with positional tagset definition
    -> T.Text        	-- ^ The tag indicating unknown words
    -> TierConf         -- ^ Tiered tagging configuration
    -> FeatSel          -- ^ Feature selection 
    -> FilePath         -- ^ Train file (plain format)
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO Disamb
train sgdArgs tagsetPath ign tierConf ftSel trainPath evalPath'Maybe = do
    _tagset <- TP.parseTagset tagsetPath <$> readFile tagsetPath
    _crf <- CRF.train sgdArgs ftSel
        (schemed _tagset ign tierConf trainPath)
        (schemed _tagset ign tierConf <$> evalPath'Maybe)
    return $ Disamb _crf _tagset tierConf

-- | Schematized data from the plain file.
schemed
    :: TP.Tagset -> T.Text -> TierConf
    -> FilePath -> IO [CRF.SentL Ob (Tag, Tag)]
schemed tagset _ign cfg =
    fmap (map onSent) . P.readPlain _ign
  where
    onSent sent =
        zip (schematize xs') (map mkDist ys')
      where
        (xs, ys) = unzip (map P.fromTok sent)
        xs' = map (mapWord   smash) xs
        ys' = map (mapChoice smash) ys
        smash = tear cfg . parseTag
        parseTag = TP.parseTag tagset
        mkDist = CRF.mkDist . M.toList . M.map unPositive
