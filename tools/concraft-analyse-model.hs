{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           System.Console.CmdArgs
import qualified Data.Map as M
import qualified Data.Number.LogFloat as L

import qualified Data.CRF.Chain2.Tiers.Model as CRF
import qualified Data.CRF.Chain2.Tiers.Feature as CRF
import qualified Data.CRF.Chain2.Tiers as CRF

import qualified NLP.Concraft as C
import qualified NLP.Concraft.Disamb as D


---------------------------------------
-- Command line options
---------------------------------------


data AnaModel = AnaModel
    { inModel   :: FilePath
    , maxVal    :: Double }
    deriving (Data, Typeable, Show)


anaModel :: AnaModel
anaModel = AnaModel
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , maxVal  = 100 &= help "Discard parameters with absolute values greater than this argument" }
    &= summary "Print Concraft model parameters in CSV format"


---------------------------------------
-- Main
---------------------------------------


-- | Get feature type identifier (T -- transition, O -- observation).
featType :: CRF.Feat -> String 
featType (CRF.OFeat _ _ _) = "O"
featType _                 = "T"


main :: IO ()
main = exec =<< cmdArgs anaModel


exec :: AnaModel -> IO ()
exec AnaModel{..} = do

    model <- CRF.model . D.crf . C.disamb <$> C.loadModel inModel

    let featMap = L.logFromLogFloat <$> CRF.toMap model :: M.Map CRF.Feat Double
        xs      = filter ((<maxVal) . abs . snd) (M.assocs featMap)

    putStrLn "type param"
    forM_ xs $ \(feat, param) -> do
        putStrLn $ featType feat ++ " " ++ show param
