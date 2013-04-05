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

import qualified NLP.Concraft.Morphosyntax as X

-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)

-- | The Ox monad specialized to word token type and text observations.
type Ox a = Ox.Ox T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
type Schema w t a = V.Vector (X.Seg w t) -> Int -> Ox a

-- | A dummy schema block.
void :: a -> Schema w t a
void x _ _ = return x

-- | Sequence the list of schemas (or blocks) and discard individual values.
sequenceS_
    :: [V.Vector (X.Seg w t) -> a -> Ox b]
    ->  V.Vector (X.Seg w t) -> a -> Ox ()
sequenceS_ xs sent =
    let ys = map ($sent) xs
    in  \k -> sequence_ (map ($k) ys)

-- | Record structure of the basic observation types.
data BaseOb = BaseOb
    { orth          :: Int -> Maybe T.Text
    , lowOrth       :: Int -> Maybe T.Text }

-- | Construct the 'BaseOb' structure given the sentence.
mkBaseOb :: X.Word w => V.Vector (X.Seg w t) -> BaseOb
mkBaseOb sent = BaseOb
    { orth      = _orth
    , lowOrth   = _lowOrth }
  where
    at          = Ox.atWith sent
    _orth       = (X.orth `at`)
    _lowOrth i  = T.toLower <$> _orth i

-- | A block is a chunk of the Ox computation performed within the
-- context of the sentence and the list of absolute sentence positions.
type Block w t a = V.Vector (X.Seg w t) -> [Int] -> Ox a

-- | Transform a block to a schema depending on
-- * A list of relative sentence positions,
-- * A boolean value; if true, the block computation
--   will be performed only on positions where an OOV
--   word resides.
fromBlock :: X.Word w => Block w t a -> [Int] -> Bool -> Schema w t a
fromBlock blk xs oovOnly sent =
    \k -> blkSent [x + k | x <- xs, oov (x + k)]
  where
    blkSent = blk sent
    oov k   = if not oovOnly
        then True
        else maybe False id $ X.oov `at` k
    at      = Ox.atWith sent

-- | Orthographic form at the current position.
orthB :: X.Word w => Block w t ()
orthB sent = \ks ->
    let orthOb = Ox.atWith sent X.orth
    in  mapM_ (Ox.save . orthOb) ks

-- | Orthographic form at the current position.
lowOrthB :: X.Word w => Block w t ()
lowOrthB sent = \ks ->
    let BaseOb{..} = mkBaseOb sent
    in  mapM_ (Ox.save . lowOrth) ks

-- | List of lowercased prefixes of given lengths.
lowPrefixesB :: X.Word w => [Int] -> Block w t ()
lowPrefixesB ns sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowPrefix i) ns
  where
    BaseOb{..}      = mkBaseOb sent
    lowPrefix i j   = Ox.prefix j =<< lowOrth i

-- | List of lowercased suffixes of given lengths.
lowSuffixesB :: X.Word w => [Int] -> Block w t ()
lowSuffixesB ns sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowSuffix i) ns
  where
    BaseOb{..}      = mkBaseOb sent
    lowSuffix i j   = Ox.suffix j =<< lowOrth i

-- | Shape of the word.
knownB :: X.Word w => Block w t ()
knownB sent = \ks -> do
    mapM_ (Ox.save . knownAt) ks
  where
    at          = Ox.atWith sent
    knownAt i   = boolF <$> (not . X.oov) `at` i
    boolF True  = "T"
    boolF False = "F"

-- | Shape of the word.
shapeB :: X.Word w => Block w t ()
shapeB sent = \ks -> do
    mapM_ (Ox.save . shape) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i

-- | Packed shape of the word.
packedB :: X.Word w => Block w t ()
packedB sent = \ks -> do
    mapM_ (Ox.save . shapeP) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i

-- | Packed shape of the word.
begPackedB :: X.Word w => Block w t ()
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
-- shapePairB :: Block w t ()
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
-- packedPairB :: Block w t ()
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

mkArg0 :: X.Word w => Block w t () -> Entry () -> Schema w t ()
mkArg0 blk (Just x) = fromBlock blk (range x) (oovOnly x)
mkArg0 _   Nothing  = void ()

mkArg1 :: X.Word w => (a -> Block w t ()) -> Entry a -> Schema w t ()
mkArg1 blk (Just x) = fromBlock (blk (args x)) (range x) (oovOnly x)
mkArg1 _   Nothing  = void ()

-- | Build the schema based on the configuration.
fromConf :: X.Word w => SchemaConf -> Schema w t ()
fromConf SchemaConf{..} = sequenceS_
    [ mkArg0 orthB orthC
    , mkArg0 lowOrthB lowOrthC
    , mkArg1 lowPrefixesB lowPrefixesC
    , mkArg1 lowSuffixesB lowSuffixesC
    , mkArg0 knownB knownC
    , mkArg0 shapeB shapeC
    , mkArg0 packedB packedC
    , mkArg0 begPackedB begPackedC ]

-- | Use the schema to extract observations from the sentence.
schematize :: Schema w t a -> X.Sent w t -> [[Ob]]
schematize schema xs =
    map (Ox.execOx . schema v) [0 .. n - 1]
  where
    v = V.fromList xs
    n = V.length v
