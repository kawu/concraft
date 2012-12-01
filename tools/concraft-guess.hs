{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import NLP.Concraft.Guess (train, tag)
import NLP.Concraft.Format.Plain (plainFormat)

-- | Data formats. 
data Format = Plain deriving (Data, Typeable, Show)

data Args
  = TrainMode
    { trainPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , format        :: Format
    -- TODO: ignore tag should be related only to the Plain
    -- format, but then 'Format' would not be an Enum instance.
    -- Try another command line parsing library?
    , ignTag        :: String
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , outGuesser    :: FilePath }
  | TagMode
    { dataPath      :: FilePath
    , format        :: Format
    , ignTag        :: String
    , inGuesser     :: FilePath
    , guessNum      :: Int }
  deriving (Data, Typeable, Show)

trainMode :: Args
trainMode = TrainMode
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , format = enum [Plain &= help "Plain format"]
    , ignTag = def &= help "Tag indicating OOV word"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outGuesser = def &= typFile &= help "Output Guesser file" }

tagMode :: Args
tagMode = TagMode
    { inGuesser = def &= argPos 0 &= typ "GUESSER-FILE"
    , dataPath = def &= argPos 1 &= typ "INPUT"
    , format = enum [Plain &= help "Plain format"]
    , ignTag = def &= help "Tag indicating OOV word"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec TrainMode{..} = do
    gsr <- case format of
        Plain   -> doTrain (plainFormat ign)
    when (not . null $ outGuesser) $ do
        putStrLn $ "\nSaving model in " ++ outGuesser ++ "..."
        encodeFile outGuesser gsr
  where
    doTrain fh = train fh sgdArgs trainPath evalPath
    ign = T.pack ignTag
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec TagMode{..} = do
    doTag <- doTagWith <$> decodeFile inGuesser <*> L.readFile dataPath
    case format of
        Plain   -> L.putStr $ doTag (plainFormat ign)
  where
    doTagWith gsr input fh = tag fh guessNum gsr input
    ign = T.pack ignTag
