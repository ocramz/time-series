module Data.TimeSeries.Forex where

import Data.TimeSeries.Forex.Types

import Data.Time


{- |

row format : open, high, low, close

-}


rateMean :: Fractional a => FxRow a -> a
rateMean r = (rateOpen r + rateClose r) / 2

rateDiff :: Fractional a => FxRow a -> a -> a
rateDiff r dt = (rateClose r - rateOpen r) / dt
