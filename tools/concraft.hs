{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import System.Console.CmdArgs
import Data.Binary (Binary, put, get, encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
import qualified Data.Tagset.Positional as P

import NLP.Concraft.Format.Plain (plainFormat)
import qualified NLP.Concraft as C
import qualified NLP.Concraft.Schema as S
import qualified NLP.Concraft.Format as F
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb.Positional as D
import qualified NLP.Concraft.Disamb.Tiered as R

-- | Data formats. 
data Format = Plain deriving (Data, Typeable, Show)

-- | Concraft data.
data ConcraftData = ConcraftData
    { guesser       :: G.Guesser F.Tag
    , disambModel   :: R.CRF S.Ob D.Part
    , tagset        :: P.Tagset
    , tierConf      :: [D.Tier] }

instance Binary ConcraftData where
    put ConcraftData{..} = do
        put guesser
        put disambModel
        put tagset
        put tierConf
    get = ConcraftData <$> get <*> get <*> get <*> get

data Concraft
  = Train
    { trainPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , format        :: Format
    -- TODO: ignore tag should be related only to the Plain
    -- format, but then 'Format' would not be an Enum instance.
    -- Try another command line parsing library?
    , tagsetPath    :: FilePath
    , ignTag        :: String
    , discardHidden :: Bool
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , outModel      :: FilePath
    , guessNum      :: Int }
  | Disamb
    { format        :: Format
    , ignTag        :: String
    , inModel       :: FilePath
    , guessNum      :: Int }
  deriving (Data, Typeable, Show)

trainMode :: Concraft
trainMode = Train
    { tagsetPath = def &= argPos 0 &= typ "TAGSET-PATH"
    , trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , format = enum [Plain &= help "Plain format"]
    , ignTag = def &= help "Tag indicating OOV word"
    , discardHidden = False &= help "Discard hidden features"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outModel = def &= typFile &= help "Output Model file"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word" }

disambMode :: Concraft
disambMode = Disamb
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , format = enum [Plain &= help "Plain format"]
    , ignTag = def &= help "Tag indicating OOV word"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word" }

argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes [trainMode, disambMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Concraft -> IO ()

exec Train{..} = do
    tagset' <- P.parseTagset tagsetPath <$> readFile tagsetPath
    (guesser', disambModel') <- case format of
        Plain   -> doTrain (plainFormat ign) tagset'
    let concraftData = ConcraftData
            { guesser       = guesser'
            , disambModel   = disambModel'
            , tagset        = tagset'
            , tierConf      = D.tierConfDefault }
    when (not . null $ outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        encodeFile outModel concraftData
  where
    doTrain docHandler tagset' = C.trainOn
        docHandler guessConf sgdArgs
        (disambTrain tagset')
        trainPath evalPath
    guessConf = C.GuessConf
        { C.guessNum = guessNum
        , C.guessSchema = S.guessSchemaDefault }
    disambTrain tagset' = C.DisambWith
        { C.disambConf = disambConf tagset'
        , C.disambWith = R.train (length D.tierConfDefault) featSel sgdArgs }
    disambConf tagset' = C.DisambConf
        { C.split = D.split D.tierConfDefault . P.parseTag tagset'
        , disambSchema = S.disambSchemaDefault }
    featSel = if discardHidden
        then R.selectPresent
        else R.selectHidden
    ign = T.pack ignTag
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec Disamb{..} = do
    doTag <- doTagWith <$> decodeFile inModel <*> L.getContents
    case format of
        Plain   -> L.putStr $ doTag (plainFormat ign)
  where
    doTagWith ConcraftData{..} input docHandler =
        let guessData = C.GuessData
                { C.guessConf = C.GuessConf
                    { C.guessNum = guessNum
                    , guessSchema = S.guessSchemaDefault }
                , C.guesser = guesser }
            disambTag = C.DisambWith
                { C.disambConf = C.DisambConf
                    { C.split = D.split tierConf . P.parseTag tagset
                    , C.disambSchema = S.disambSchemaDefault }
                , C.disambWith = R.tag disambModel }
        in  C.disambDoc docHandler guessData disambTag input
    ign = T.pack ignTag
