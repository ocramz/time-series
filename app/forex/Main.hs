{-# language FlexibleContexts #-}
module Main where

import Lib

-- import Chart.Unit
-- import Chart.Types
import Control.Monad (forM, forM_)
import Control.Monad.Trans.State
import Control.Monad.State.Class
import Control.Monad.Catch

import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA

import Data.Text
import qualified Data.Text.IO as T

import Data.Time

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

import System.Environment (getArgs, withArgs)

import qualified Data.Attoparsec.Text as A

import Control.Arrow ((&&&))

fname :: String
fname = "data/forex/EURCHF_hour.csv"

main :: IO ()
main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right (FxDataset cp datarows) ->
               mainWith (timeSeriesPlot (show cp) $ open <$> datarows)

open, high, low, close :: FxRow a -> (Double, a)
open = toRealTS rateOpen
high = toRealTS rateHigh
low = toRealTS rateLow
close = toRealTS rateClose

toRealTS :: (FxRow a -> b) -> FxRow a -> (Double, b)
toRealTS f = dayToNum . date &&& f

dayToNum :: Day -> Double
dayToNum = fromIntegral . fromEnum . toModifiedJulianDay

-- asdf :: MonadThrow m => ConduitM Text o m (FxDataSet Double)
-- asdf = CA.sinkParser parseFxDataset


linePlots ds = mconcat <$> forM ds plotm where
  plotm (d, col) = 
    linePlot d $ do
      plotColor .= col
      lineStyle %= lwN 0.001



timeSeriesPlot :: String -> [(Double, Double)] -> IO (Axis B V2 Double)
timeSeriesPlot descStr d = execStateT ?? r2Axis $ do
        -- xMin ?= 0
        -- xMax ?= fromIntegral (Prelude.length d)
        linePlot d $ do
          -- key descStr
          plotColor .= blue
          lineStyle %= lwN 0.00001
        legendStyle . _lw .= 0
        legendTextWidth *= 4
          -- lineStyle %= (dashingG [0.3, 0.5] 0 #
          --               lwN 0.01)


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
