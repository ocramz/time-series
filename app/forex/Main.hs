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

-- import Data.Text
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
fname = "data/forex/EURUSD_hour.csv"


main :: IO ()
main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right (FxDataset _ datarows) ->
               mainWith $ 
                 timeSeriesPlots ds where
                   npoints = 24 * 5
                   ds = [
                     -- (data_open, green),
                     (data_hi, red),
                     (data_lo, blue),
                     (data_minmaxdiff, black)
                     -- (data_close, black)
                     ]
                   -- data_open = take npoints $ open <$> datarows
                   data_hi = take npoints $ high <$> datarows
                   data_lo = take npoints $ low <$> datarows
                   -- data_close = take npoints $ close <$> datarows
                   data_minmaxdiff = uncurry zip (t_, zipWith (\x y -> 1.11 + x - y) (snd <$> d2) (snd <$> d1))
                     where
                        d2 = data_hi
                        d1 = data_lo
                        t_ = fst <$> d1
                 -- timeSeriesPlot (show cp) $ open <$> datarows

open, high, low, close :: FxRow a -> (Double, a)
open = toRealTS rateOpen
high = toRealTS rateHigh
low = toRealTS rateLow
close = toRealTS rateClose

toRealTS :: (FxRow a -> b) -> FxRow a -> (Double, b)
toRealTS f = (dateTimeToNum <$> date <*> timeOfDay) &&& f 
  

-- toRealTS :: (FxRow a -> b) -> FxRow a -> (Double, b)
-- toRealTS f = dayToNum . date &&& f

-- dayToNum :: Day -> Double
-- dayToNum = fromIntegral . fromEnum . toModifiedJulianDay

dateTimeToNum :: Num a => Day -> TimeOfDay -> a
dateTimeToNum dd tt = fromIntegral $ d + t where
  d = 24 * (fromEnum $ toModifiedJulianDay dd)
  t = todHour tt

-- asdf :: MonadThrow m => ConduitM Text o m (FxDataSet Double)
-- asdf = CA.sinkParser parseFxDataset

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
