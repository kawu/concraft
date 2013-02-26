{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
import qualified Data.Tagset.Positional as P

import NLP.Concraft.Format.Plain (plainFormat)
import qualified NLP.Concraft as C
import qualified NLP.Concraft.Schema as S
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D

-- | Data formats. 
data Format = Plain deriving (Data, Typeable, Show)

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
    -- , discardHidden :: Bool
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
    , inModel       :: FilePath }
    -- , guessNum      :: Int }
  deriving (Data, Typeable, Show)

trainMode :: Concraft
trainMode = Train
    { tagsetPath = def &= argPos 0 &= typ "TAGSET-PATH"
    , trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , format = enum [Plain &= help "Plain format"]
    , ignTag = "ign" &= help "Tag indicating OOV word"
    -- , discardHidden = False &= help "Discard hidden features"
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
    , ignTag = "ign" &= help "Tag indicating OOV word" }
    -- , guessNum = 10 &= help "Number of guessed tags for each unknown word" }

argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes [trainMode, disambMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Concraft -> IO ()

exec Train{..} = do
    tagset <- P.parseTagset tagsetPath <$> readFile tagsetPath
    concraft <- case format of
        Plain   -> train (plainFormat ign) tagset
    when (not . null $ outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        encodeFile outModel concraft
  where
    train docH tagset =
        let guessConf  = G.TrainConf S.guessConfDefault sgdArgs
            disambConf = D.TrainConf tagset D.tiersDefault
                S.disambConfDefault sgdArgs
        in  C.train docH guessNum guessConf disambConf trainPath evalPath 
    ign = T.pack ignTag
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec Disamb{..} = do
    tag <- tagWith <$> decodeFile inModel <*> L.getContents
    case format of
        Plain   -> L.putStr $ tag (plainFormat ign)
  where
    tagWith concraft input docH = C.tagDoc docH concraft input
    ign = T.pack ignTag
