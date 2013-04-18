{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Monad (void)
import           System.Console.CmdArgs
import           Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Number.LogFloat as L

import           Data.Colour.Names
import           Data.Colour
import           Data.Accessor
import           Graphics.Rendering.Chart

import qualified Data.CRF.Chain2.Tiers.Model as CRF
import qualified Data.CRF.Chain2.Tiers.Feature as CRF
import qualified Data.CRF.Chain2.Tiers as CRF

import qualified NLP.Concraft as C
import qualified NLP.Concraft.Disamb as D


---------------------------------------
-- Histogram
---------------------------------------


-- | Round double value.
roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)


-- | Make a histogram from input list.
hist :: Ord a => [a] -> M.Map a Int
hist =
    let update m x = M.insertWith' (+) x 1 m
    in  foldl' update M.empty


---------------------------------------
-- Rendering
---------------------------------------


drawModel :: Int -> CRF.Model -> FilePath -> IO ()
drawModel rnParam model filePath = do

    void $ renderableToPNGFile (toRenderable layout) 640 480 filePath

  where

    -- Feature map with values in log domain
    featMap = L.logFromLogFloat <$> CRF.toMap model

    -- Values assigned to observation features
    obVals = [x | (ft, x) <- M.assocs featMap, isOFeat ft]

    -- Values assigned to transition features
    trVals = [x | (ft, x) <- M.assocs featMap, not (isOFeat ft)]

    -- Is it an observation feature?
    isOFeat (CRF.OFeat _ _ _) = True
    isOFeat _                 = False

    -- Make log-domain histogram
    mkHist = map (second intLog) . M.toList . hist . map (roundTo rnParam)

    obChart =
          plot_lines_style .> line_color ^= opaque blue
        $ plot_lines_values ^= [mkHist obVals]
        $ plot_lines_title ^= "Observation features"
        $ defaultPlotLines

    trChart =
          plot_lines_style .> line_color ^= opaque green
        $ plot_lines_values ^= [mkHist trVals]
        $ plot_lines_title ^= "Transition features"
        $ defaultPlotLines

    layout =
          layout1_left_axis ^: laxis_override ^= axisGridHide
        $ layout1_right_axis ^: laxis_override ^= axisGridHide
        $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
        $ layout1_plots ^= [Left (toPlot obChart), Right (toPlot trChart)]
        $ layout1_grid_last ^= False
        $ defaultLayout1


-- | Int logarithm.
intLog :: Int -> Double
intLog = (log :: Double -> Double) . fromIntegral


---------------------------------------
-- Command line options
---------------------------------------


data AnaModel = AnaModel
    { inModel   :: FilePath
    , outFile   :: FilePath
    , rnParam   :: Int }
    deriving (Data, Typeable, Show)


anaModel :: AnaModel
anaModel = AnaModel
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , outFile = def &= argPos 1 &= typ "OUTPUT-FILE"
    , rnParam = 1 &= help "Rounding parameter" }


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs anaModel


exec :: AnaModel -> IO ()
exec AnaModel{..} = do
    model <- CRF.model . D.crf . C.disamb <$> C.loadModel inModel
    drawModel rnParam model outFile
