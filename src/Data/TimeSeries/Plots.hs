{-# language FlexibleContexts #-}
module Data.TimeSeries.Plots where

import Control.Monad (forM)
import Control.Monad.Trans.State
import Data.Typeable

import Plots
import Plots.Axis
import Plots.Axis.Render
import Plots.Legend
import Plots.Style
import Plots.Types
import Plots.Types.Histogram
import Plots.Types.Line

import Diagrams.Prelude
-- import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Postscript
import Diagrams.Backend.Postscript.CmdLine


-- * Plotting helpers

-- timeSeriesPlots :: Foldable f => f ([(Double, Double)], Color Double) -> IO (Axis B V2 Double)
timeSeriesPlots :: [([(Double, Double)], Colour Double)] -> IO (Axis B V2 Double)
timeSeriesPlots ds = execStateT ?? r2Axis $ mconcat <$> forM ds plotm where
  plotm (d, col) = 
    linePlot d $ do
      plotColor .= col
      lineStyle %= lwN 0.00001



-- timeSeriesPlot :: String -> [(Double, Double)] -> IO (Axis B V2 Double)
-- timeSeriesPlot _ d = execStateT ?? r2Axis $ do
--         -- xMin ?= 0
--         -- xMax ?= fromIntegral (Prelude.length d)
--         linePlot d $ do
--           -- key descStr
--           plotColor .= blue
--           lineStyle %= lwN 0.00001
--         legendStyle . _lw .= 0
--         legendTextWidth *= 4
--           -- lineStyle %= (dashingG [0.3, 0.5] 0 #
--           --               lwN 0.01)


histPlot :: String -> [Double] -> IO (Axis B V2 Double)
histPlot descStr d = execStateT ?? r2Axis $ do
       histogramPlot d $ do
         -- key descStr
         plotColor .= blue
         areaStyle . _opacity .= 0.5
         numBins .= 50
         normaliseSample .= pdf
       legendStyle . _lw .= 0
       legendTextWidth *= 4
