module Data.TimeSeries.Forex.Types where

import Data.Time (Day, TimeOfDay)



data Currency = EUR | JPY | GBP | USD | CHF | AUD deriving (Eq, Show)

data CurrencyPair = CPair { numerator :: Currency
                          , denominator :: Currency } deriving (Eq, Show)

data FxRow a  = FxRow {
    currencyPair :: CurrencyPair
  , date :: Day
  , timeOfDay :: TimeOfDay
  , rateOpen :: a
  , rateHigh :: a
  , rateLow :: a
  , rateClose :: a
               } deriving (Eq, Show)
