module Data.TimeSeries.Forex.Utils where

import Data.TimeSeries.Forex.Types

import Control.Arrow ((&&&))
import Data.Time

-- * Accessors for the FxRow type

open, high, low, close :: FxRow a -> (Double, a)
open = toRealTS rateOpen
high = toRealTS rateHigh
low = toRealTS rateLow
close = toRealTS rateClose

toRealTS :: (FxRow a -> b) -> FxRow a -> (Double, b)
toRealTS f = (dateTimeToNum <$> date <*> timeOfDay) &&& f 
  


-- | Convert a `Day` and a `TimeOfDay` into a monotonically increasing number (dirty hack for plotting time series)
dateTimeToNum :: Num a => Day -> TimeOfDay -> a
dateTimeToNum dd tt = fromIntegral $ d + t where
  d = 24 * (fromEnum $ toModifiedJulianDay dd)
  t = todHour tt
