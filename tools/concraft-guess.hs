{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import NLP.Concraft.Guess (learn, tagFile)

data Args
  = LearnMode
    { learnPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , ignTag        :: String
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , outGuesser    :: FilePath }
  | TagMode
    { dataPath      :: FilePath
    , inGuesser     :: FilePath
    , guessNum      :: Int }
  deriving (Data, Typeable, Show)

learnMode :: Args
learnMode = LearnMode
    { ignTag = def &= argPos 0 &= typ "IGN-TAG"
    , learnPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
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
    , guessNum = 10 &= help "Number of guessed tags for each unknown word" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [learnMode, tagMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec LearnMode{..} = do
    gsr <- learn sgdArgs (T.pack ignTag) learnPath evalPath
    when (not . null $ outGuesser) $ do
        putStrLn $ "\nSaving model in " ++ outGuesser ++ "..."
        encodeFile outGuesser gsr
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec TagMode{..} = do
    gsr <- decodeFile inGuesser
    L.putStr =<< tagFile guessNum gsr dataPath
