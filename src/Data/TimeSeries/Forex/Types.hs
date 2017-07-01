module Data.TimeSeries.Forex.Types where

import Data.Time -- (TimeOfDay(..), Day)



data Currency = EUR | JPY | GBP | USD | CHF | AUD deriving (Eq, Show)

data CurrencyPair = CPair { numerator :: Currency
                          , denominator :: Currency } deriving (Eq, Show)

data FxRow a  = FxRow {
    date :: Day
  , timeOfDay :: TimeOfDay
  , rateOpen :: a
  , rateHigh :: a
  , rateLow :: a
  , rateClose :: a
               } deriving (Eq, Show)

data FxDataSet a = FxDataset { currencyPair :: CurrencyPair
                             , dataRows :: [FxRow a] } deriving (Eq, Show)


-- time handling

-- timeToEnum (Day d) (TimeOfDay hh mm ss) = undefined




-- --

data Eur = Eur deriving (Eq, Show)
data Jpy = Jpy deriving (Eq, Show)
data Gbp = Gbp deriving (Eq, Show)
data Usd = Usd deriving (Eq, Show)
data Chf = Chf deriving (Eq, Show)
data Aud = Aud deriving (Eq, Show)

newtype FxDataset' c1 c2 a = FxDataset' (FxDataSet a) deriving (Eq, Show)



type EurUsd a = FxDataset' Eur Usd a
