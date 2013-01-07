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

import NLP.Concraft.Disamb
import NLP.Concraft.Format.Plain (plainFormat)
import NLP.Concraft.Disamb.Positional
import qualified NLP.Concraft.Disamb.Tiered as R

-- | Data formats. 
data Format = Plain deriving (Data, Typeable, Show)

-- | Disambiguation model data.
data Disamb = Disamb
    { crf       :: R.CRF Ob Part
    , tagset    :: P.Tagset
    , tierConf  :: [Tier] }

instance Binary Disamb where
    put Disamb{..} = put crf >> put tagset >> put tierConf
    get = Disamb <$> get <*> get <*> get

data Args
  = TrainMode
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
    , outDisamb     :: FilePath }
  | TagMode
    { dataPath      :: FilePath
    , format        :: Format
    , ignTag        :: String
    , inDisamb      :: FilePath }
  deriving (Data, Typeable, Show)

trainMode :: Args
trainMode = TrainMode
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
    , outDisamb = def &= typFile &= help "Output Disamb file" }

tagMode :: Args
tagMode = TagMode
    { inDisamb = def &= argPos 0 &= typ "DISAMB-FILE"
    , dataPath = def &= argPos 1 &= typ "INPUT"
    , format = enum [Plain &= help "Plain format"]
    , ignTag = def &= help "Tag indicating OOV word" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec TrainMode{..} = do
    tagset' <- P.parseTagset tagsetPath <$> readFile tagsetPath
    crf' <- case format of
        Plain   -> doTrain (plainFormat ign) tagset'
    let dmb = Disamb crf' tagset' tierConfDefault
    when (not . null $ outDisamb) $ do
        putStrLn $ "\nSaving model in " ++ outDisamb ++ "..."
        encodeFile outDisamb dmb
  where
    doTrain docHandler tagset' = trainOn
        docHandler schemaDefault
        (split tierConfDefault . P.parseTag tagset')
        (R.train (length tierConfDefault) featSel sgdArgs)
        trainPath evalPath
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

exec TagMode{..} = do
    doTag <- doTagWith <$> decodeFile inDisamb <*> L.readFile dataPath
    case format of
        Plain   -> L.putStr $ doTag (plainFormat ign)
  where
    doTagWith Disamb{..} input docHandler = disambDoc
    	docHandler schemaDefault
        (split tierConf . P.parseTag tagset)
        (R.tag crf) input
    ign = T.pack ignTag
