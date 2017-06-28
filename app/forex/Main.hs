module Main where

import Lib

-- import Chart.Unit
-- import Chart.Types
import Control.Monad.Trans.State
import Control.Monad.Catch

import Data.Conduit
import Data.Text
import qualified Data.Conduit.Attoparsec as CA

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

import System.Environment (getArgs, withArgs)


main = putStrLn "hello!"



asdf :: MonadThrow m => ConduitM Text o m (FxDataSet Double)
asdf = CA.sinkParser parseFxDataset


timeSeriesPlot :: String -> [(Double, Double)] -> IO (Axis B V2 Double)
timeSeriesPlot descStr d = execStateT ?? r2Axis $ do
        xMin ?= 0
        xMax ?= fromIntegral (Prelude.length d)
        linePlot d $ do
          -- -- key descStr
          plotColor .= blue
          lineStyle %= lwN 0.0001
        legendStyle . _lw .= 0
        legendTextWidth *= 4
          -- lineStyle %= (dashingG [0.3, 0.5] 0 #
          --               lwN 0.01)
