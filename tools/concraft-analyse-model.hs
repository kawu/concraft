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
import qualified Data.CRF.Chain2.Tiers as CRF

import qualified NLP.Concraft as C
import qualified NLP.Concraft.Disamb as D


---------------------------------------
-- Histogram
---------------------------------------


-- | Round double value.
roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)


-- | Make a histogram with a given precision.
hist :: Ord a => [a] -> M.Map a Int
hist =
    let update m x = M.insertWith' (+) x 1 m
    in  foldl' update M.empty


---------------------------------------
-- Rendering
---------------------------------------


drawModel :: CRF.Model -> FilePath -> IO ()
drawModel model filePath = do

    void $ renderableToPNGFile (toRenderable layout) 640 480 filePath

  where

    -- Feature values in log domain
    vals = map L.logFromLogFloat . M.elems $ CRF.toMap model

    -- Make log-domain histogram
    xs = map (second intLog) . M.toList . hist $ map (roundTo 2) vals

    chart =
          plot_lines_style .> line_color ^= opaque blue
        $ plot_lines_values ^= [xs]
        $ defaultPlotLines

    layout =
          layout1_left_axis ^: laxis_override ^= axisGridHide
        $ layout1_right_axis ^: laxis_override ^= axisGridHide
        $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
        $ layout1_plots ^= [Left (toPlot chart)]
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
    , outFile   :: FilePath }
    deriving (Data, Typeable, Show)


anaModel :: AnaModel
anaModel = AnaModel
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , outFile = def &= argPos 1 &= typ "OUTPUT-FILE" }


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs anaModel


exec :: AnaModel -> IO ()
exec AnaModel{..} = do
    model <- CRF.model . D.crf . C.disamb <$> C.loadModel inModel
    drawModel model outFile
