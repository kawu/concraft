{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Observation schema blocks for Concraft.

module NLP.Concraft.Schema
( 
-- * Types
  Ob
, Ox
, Schema
, void
, sequenceS_

-- * Usage
, schematize

-- * Configuration
, Body (..)
, Entry
, entry
, entryWith
, SchemaConf (..)
, nullConf
, fromConf

, guessConfDefault
, disambConfDefault

-- * Schema blocks
, Block
, fromBlock
, orthB
, lowOrthB
, lowPrefixesB
, lowSuffixesB
, knownB
, shapeB
, packedB
, begPackedB
) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM_)
import Data.Binary (Binary, put, get)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox

import qualified NLP.Concraft.Morphosyntax as Mx

-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)

-- | The Ox monad specialized to word token type and text observations.
type Ox t a = Ox.Ox (Mx.Word t) T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
type Schema t a = V.Vector (Mx.Word t) -> Int -> Ox t a

-- | A dummy schema block.
void :: a -> Schema t a
void x _ _ = return x

-- | Sequence the list of schemas (or blocks) and discard individual values.
sequenceS_
    :: [V.Vector (Mx.Word t) -> a -> Ox t b]
    ->  V.Vector (Mx.Word t) -> a -> Ox t ()
sequenceS_ xs sent =
    let ys = map ($sent) xs
    in  \k -> sequence_ (map ($k) ys)

-- | Record structure of the basic observation types.
data BaseOb = BaseOb
    { orth          :: Int -> Maybe T.Text
    , lowOrth       :: Int -> Maybe T.Text }

-- | Construct the 'BaseOb' structure given the sentence.
mkBaseOb :: V.Vector (Mx.Word t) -> BaseOb
mkBaseOb sent = BaseOb
    { orth      = _orth
    , lowOrth   = _lowOrth }
  where
    at          = Ox.atWith sent
    _orth       = (Mx.orth `at`)
    _lowOrth i  = T.toLower <$> _orth i

-- | A block is a chunk of the Ox computation performed within the
-- context of the sentence and the list of absolute sentence positions.
type Block t a = V.Vector (Mx.Word t) -> [Int] -> Ox t a

-- | Transform a block to a schema depending on
-- * A list of relative sentence positions,
-- * A boolean value; if true, the block computation
--   will be performed only on positions where an OOV
--   word resides.
fromBlock :: Block t a -> [Int] -> Bool -> Schema t a
fromBlock blk xs oovOnly sent =
    \k -> blkSent [x + k | x <- xs, oov (x + k)]
  where
    blkSent = blk sent
    oov k   = if not oovOnly
        then True
        else maybe False id $ Mx.oov `at` k
    at      = Ox.atWith sent

-- | Orthographic form at the current position.
orthB :: Block t ()
orthB sent = \ks ->
    let orthOb = Ox.atWith sent Mx.orth
    in  mapM_ (Ox.save . orthOb) ks

-- | Orthographic form at the current position.
lowOrthB :: Block t ()
lowOrthB sent = \ks ->
    let BaseOb{..} = mkBaseOb sent
    in  mapM_ (Ox.save . lowOrth) ks

-- | List of lowercased prefixes of given lengths.
lowPrefixesB :: [Int] -> Block t ()
lowPrefixesB ns sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowPrefix i) ns
  where
    BaseOb{..}      = mkBaseOb sent
    lowPrefix i j   = Ox.prefix j =<< lowOrth i

-- | List of lowercased suffixes of given lengths.
lowSuffixesB :: [Int] -> Block t ()
lowSuffixesB ns sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowSuffix i) ns
  where
    BaseOb{..}      = mkBaseOb sent
    lowSuffix i j   = Ox.suffix j =<< lowOrth i

-- | Shape of the word.
knownB :: Block t ()
knownB sent = \ks -> do
    mapM_ (Ox.save . knownAt) ks
  where
    at          = Ox.atWith sent
    knownAt i   = boolF <$> (not . Mx.oov) `at` i
    boolF True  = "T"
    boolF False = "F"

-- | Shape of the word.
shapeB :: Block t ()
shapeB sent = \ks -> do
    mapM_ (Ox.save . shape) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i

-- | Packed shape of the word.
packedB :: Block t ()
packedB sent = \ks -> do
    mapM_ (Ox.save . shapeP) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i

-- | Packed shape of the word.
begPackedB :: Block t ()
begPackedB sent = \ks -> do
    mapM_ (Ox.save . begPacked) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i
    begPacked i     = isBeg i <> pure "-" <> shapeP i
    isBeg i         = (Just . boolF) (i == 0)
    boolF True      = "T"
    boolF False     = "F"
    x <> y          = T.append <$> x <*> y

-- -- | Combined shapes of two consecutive (at @k-1@ and @k@ positions) words.
-- shapePairB :: Block t ()
-- shapePairB sent = \ks ->
--     forM_ ks $ \i -> do
--         Ox.save $ link <$> shape  i <*> shape  (i - 1)
--   where
--     BaseOb{..}      = mkBaseOb sent
--     shape i         = Ox.shape <$> orth i
--     link x y        = T.concat [x, "-", y]
-- 
-- -- | Combined packed shapes of two consecutive (at @k-1@ and @k@ positions)
-- -- words.
-- packedPairB :: Block t ()
-- packedPairB sent = \ks ->
--     forM_ ks $ \i -> do
--         Ox.save $ link <$> shapeP i <*> shapeP (i - 1)
--   where
--     BaseOb{..}      = mkBaseOb sent
--     shape i         = Ox.shape <$> orth i
--     shapeP i        = Ox.pack <$> shape i
--     link x y        = T.concat [x, "-", y]

-- | Body of configuration entry.
data Body a = Body {
    -- | Range argument for the schema block. 
      range     :: [Int]
    -- | When true, the entry is used only for oov words.
    , oovOnly   :: Bool
    -- | Additional arguments for the schema block.
    , args      :: a }
    deriving (Show)

instance Binary a => Binary (Body a) where
    put Body{..} = put range >> put oovOnly >> put args
    get = Body <$> get <*> get <*> get

-- | Maybe entry.
type Entry a = Maybe (Body a)

-- | Entry with additional arguemnts.
entryWith :: a -> [Int] -> Entry a
entryWith v xs = Just (Body xs False v)

-- | Plain entry with no additional arugments.
entry :: [Int] -> Entry ()
entry = entryWith ()

-- | Configuration of the schema.  All configuration elements specify the
-- range over which a particular observation type should be taken on account.
-- For example, the @[-1, 0, 2]@ range means that observations of particular
-- type will be extracted with respect to previous (@k - 1@), current (@k@)
-- and after the next (@k + 2@) positions when identifying the observation
-- set for position @k@ in the input sentence.
data SchemaConf = SchemaConf {
    -- | The 'orthB' schema block.
      orthC             :: Entry ()
    -- | The 'lowOrthB' schema block.
    , lowOrthC          :: Entry ()
    -- | The 'lowPrefixesB' schema block.  The first list of ints
    -- represents lengths of prefixes.
    , lowPrefixesC      :: Entry [Int]
    -- | The 'lowSuffixesB' schema block.  The first list of ints
    -- represents lengths of suffixes.
    , lowSuffixesC      :: Entry [Int]
    -- | The 'knownB' schema block.
    , knownC            :: Entry ()
    -- | The 'shapeB' schema block.
    , shapeC            :: Entry ()
    -- | The 'packedB' schema block.
    , packedC            :: Entry ()
    -- | The 'begPackedB' schema block.
    , begPackedC         :: Entry ()
    } deriving (Show)

instance Binary SchemaConf where
    put SchemaConf{..} = do
        put orthC
        put lowOrthC
        put lowPrefixesC
        put lowSuffixesC
        put knownC
        put shapeC
        put packedC
        put begPackedC
    get = SchemaConf
        <$> get <*> get <*> get <*> get
        <*> get <*> get <*> get <*> get

-- | Null configuration of the observation schema.
nullConf :: SchemaConf
nullConf = SchemaConf
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing

mkArg0 :: Block t () -> Entry () -> Schema t ()
mkArg0 blk (Just x) = fromBlock blk (range x) (oovOnly x)
mkArg0 _   Nothing  = void ()

mkArg1 :: (a -> Block t ()) -> Entry a -> Schema t ()
mkArg1 blk (Just x) = fromBlock (blk (args x)) (range x) (oovOnly x)
mkArg1 _   Nothing  = void ()

-- | Build the schema based on the configuration.
fromConf :: SchemaConf -> Schema t ()
fromConf SchemaConf{..} = sequenceS_
    [ mkArg0 orthB orthC
    , mkArg0 lowOrthB lowOrthC
    , mkArg1 lowPrefixesB lowPrefixesC
    , mkArg1 lowSuffixesB lowSuffixesC
    , mkArg0 knownB knownC
    , mkArg0 shapeB shapeC
    , mkArg0 packedB packedC
    , mkArg0 begPackedB begPackedC ]

-- -- | Use the schema to extract observations from the sentence.
-- schematize :: Schema t a -> [Mx.Word t] -> CRF.Sent Ob
-- schematize schema xs =
--     map (S.fromList . Ox.execOx . schema v) [0 .. n - 1]
--   where
--     v = V.fromList xs
--     n = V.length v

---------------------------------
-- Default schema configurations.
---------------------------------

-- | Default configuration for the guessing observation schema.
guessConfDefault :: SchemaConf
guessConfDefault = nullConf
    { lowPrefixesC  = entryWith [1, 2]      [0]
    , lowSuffixesC  = entryWith [1, 2]      [0]
    , knownC        = entry                 [0]
    , begPackedC    = entry                 [0] }

-- -- | Default guessing schema.
-- guessSchemaDefault :: Schema t ()
-- guessSchemaDefault sent = \k -> do
--     mapM_ (Ox.save . lowPref k) [1, 2]
--     mapM_ (Ox.save . lowSuff k) [1, 2]
--     Ox.save (knownAt k)
--     Ox.save (isBeg k <> pure "-" <> shapeP k)
--   where
--     at          = Ox.atWith sent
--     lowOrth i   = T.toLower <$> Mx.orth `at` i
--     lowPref i j = Ox.prefix j =<< lowOrth i
--     lowSuff i j = Ox.suffix j =<< lowOrth i
--     shape i     = Ox.shape <$> Mx.orth `at` i
--     shapeP i    = Ox.pack <$> shape i
--     knownAt i   = boolF <$> (not . Mx.oov) `at` i
--     isBeg i     = (Just . boolF) (i == 0)
--     boolF True  = "T"
--     boolF False = "F"
--     x <> y      = T.append <$> x <*> y

-- | Default configuration for the guessing observation schema.
disambConfDefault :: SchemaConf
disambConfDefault = nullConf
    { lowOrthC      = entry                         [-1, 0, 1]
    , lowPrefixesC  = oov $ entryWith [1, 2, 3]     [0]
    , lowSuffixesC  = oov $ entryWith [1, 2, 3]     [0]
    , begPackedC    = oov $ entry                   [0] }
  where
    oov (Just body) = Just $ body { oovOnly = True }
    oov Nothing     = Nothing

-- -- | Default disambiguation schema.
-- disambSchemaDefault :: Schema t ()
-- disambSchemaDefault sent = \k -> do
--     mapM_ (Ox.save . lowOrth) [k - 1, k, k + 1]
--     _ <- Ox.whenJT (Mx.oov `at` k) $ do
--         mapM_ (Ox.save . lowPref k) [1, 2, 3]
--         mapM_ (Ox.save . lowSuff k) [1, 2, 3]
--         Ox.save (isBeg k <> pure "-" <> shapeP k)
--     return ()
--   where
--     at          = Ox.atWith sent
--     lowOrth i   = T.toLower <$> Mx.orth `at` i
--     lowPref i j = Ox.prefix j =<< lowOrth i
--     lowSuff i j = Ox.suffix j =<< lowOrth i
--     shape i     = Ox.shape <$> Mx.orth `at` i
--     shapeP i    = Ox.pack <$> shape i
--     isBeg i     = (Just . boolF) (i == 0)
--     boolF True  = "T"
--     boolF False = "F"
--     x <> y      = T.append <$> x <*> y

-- | Use the schema to extract observations from the sentence.
schematize :: Schema t a -> Mx.Sent t -> [[Ob]]
schematize schema xs =
    map (Ox.execOx . schema v) [0 .. n - 1]
  where
    v = V.fromList xs
    n = V.length v
