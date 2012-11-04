{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import NLP.Concraft.Disamb

tierConf :: TierConf
tierConf =
    (tier1, tier2)
  where
    tier1 = Tier True $ S.fromList ["cas", "per"]
    tier2 = Tier False $ S.fromList
        [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
        , "acn", "ppr", "agg", "vlc", "dot" ]

data Args
  = LearnMode
    { learnPath	    :: FilePath
    , evalPath      :: Maybe FilePath
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
    , ignTag        :: String
    , inDisamb      :: FilePath }
  deriving (Data, Typeable, Show)

learnMode :: Args
learnMode = LearnMode
    { tagsetPath = def &= argPos 0 &= typ "TAGSET-PATH"
    , ignTag = def &= argPos 1 &= typ "IGN-TAG"
    , learnPath = def &= argPos 2 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , discardHidden = False &= help "Discard hidden features"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outDisamb = def &= typFile &= help "Output Disamb file" }

tagMode :: Args
tagMode = TagMode
    { ignTag = def &= argPos 0 &= typ "IGN-TAG"
    , inDisamb = def &= argPos 1 &= typ "DISAMB-FILE"
    , dataPath = def &= argPos 2 &= typ "INPUT" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [learnMode, tagMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec LearnMode{..} = do
    let sel = if discardHidden then selectPresent else selectHidden
    dmb <- learn sgdArgs tagsetPath (T.pack ignTag)
                 tierConf sel learnPath evalPath
    when (not . null $ outDisamb) $ do
        putStrLn $ "\nSaving model in " ++ outDisamb ++ "..."
        encodeFile outDisamb dmb
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec TagMode{..} = do
    dmb <- decodeFile inDisamb
    L.putStr =<< tagFile (T.pack ignTag) dmb dataPath
