{-# LANGUAGE OverloadedStrings #-}

module NLP.Concraft.Guess
( Ox
, Schema
, Ob
, schema
, schematize
, Guesser
, guess
, tagFile
, learn
) where

import Control.Applicative (pure, (<$>), (<*>))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Vector as V

import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox
import qualified Data.CRF.Chain1.Constrained as CRF
import qualified Numeric.SGD as SGD

import NLP.Concraft.Morphosyntax
import qualified NLP.Concraft.Plain as P

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
schematize :: Ord t => Sent t -> CRF.Sent Ob t
schematize sent =
    [ CRF.Word (obs i) (lbs i)
    | i <- [0 .. n - 1] ]
  where
    v = V.fromList sent
    n = V.length v
    obs = S.fromList . Ox.execOx . schema v
    lbs = tags . (v V.!)

-- | A guesser represented by the conditional random field.
data Guesser t = Guesser
    { crf   :: CRF.CRF Ob t -- ^ The CRF model
    , ign   :: t            -- ^ The tag indicating unkown words
    }

-- | Determine the 'k' most probable labels for each unknown word
-- in the sentence.
guess :: Ord t => Int -> Guesser t -> Sent t -> [[t]]
guess k gsr sent = CRF.tagK k (crf gsr) (schematize sent)
{-# INLINE guess #-}

-- | Tag the file.
tagFile
    :: Int              -- ^ Guesser argument
    -> Guesser T.Text   -- ^ Guesser itself
    -> FilePath         -- ^ File to tag (plain format)
    -> IO L.Text
tagFile k gsr path =
    P.showPlain (ign gsr) . map onSent <$> P.readPlain (ign gsr) path
  where
    onSent sent =
        let (xs, _) = unzip (map P.fromTok sent)
            yss = guess k gsr xs
        in  [ if P.known tok
                then tok
                else P.addNones False tok ys
            | (tok, ys) <- zip sent yss ]

-- | TODO: Abstract over the format type.
learn
    :: SGD.SgdArgs      -- ^ SGD parameters 
    -> T.Text        	-- ^ The tag indicating unknown words
    -> FilePath         -- ^ Train file (plain format)
    -> Maybe FilePath   -- ^ Maybe eval file
    -> IO (Guesser T.Text)
learn sgdArgs _ign trainPath evalPath'Maybe = do
    _crf <- CRF.train sgdArgs
        (schemed _ign trainPath)
        (schemed _ign <$> evalPath'Maybe)
        (const CRF.presentFeats)
    return $ Guesser _crf _ign

-- | Schematized data from the plain file.
schemed :: T.Text -> FilePath -> IO [CRF.SentL Ob T.Text]
schemed _ign =
    fmap (map onSent) . P.readPlain _ign
  where
    onSent sent =
        let (xs, ys) = unzip (map P.fromTok sent)
            mkDist = CRF.mkDist . M.toList . M.map unPositive
        in  zip (schematize xs) (map mkDist ys)
