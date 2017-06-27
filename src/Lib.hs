module Lib (module X) where

import Lib.Parsers.Forex as X




rateMean :: Fractional a => FxRow a -> a
rateMean r = (rateOpen r + rateClose r) / 2

rateDiff :: Fractional a => FxRow a -> a -> a
rateDiff r dt = (rateClose r - rateOpen r) / dt
