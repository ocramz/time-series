{-# language OverloadedStrings #-}
module Lib.Parsers.HistoricalFX where

import Control.Applicative ((<|>))

import qualified Data.Attoparsec.Internal.Types as A
import Data.Attoparsec.Text hiding (space)
import Data.Text
import Data.Time
import Attoparsec.Time


-- GBPJPY,2017-06-26 21:00:00,142.3470,142.3520,142.2970,142.3140

data Currency = EUR | JPY | GBP | USD | CHF deriving (Eq, Show)

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



parseCurrencyPair :: A.Parser Text CurrencyPair
parseCurrencyPair =
  (string "GBPJPY" *> pure (CPair GBP JPY) ) <|>
  (string "EURUSD" *> pure (CPair EUR USD) )

parseDateTime :: A.Parser Text (Day, TimeOfDay)
parseDateTime = do
  d <- dayInISO8601
  _ <- space
  t <- timeOfDayInISO8601
  return (d, t)

parseFxRow :: A.Parser Text (FxRow Double)
parseFxRow = do
  cp <- parseCurrencyPair
  _ <- comma
  (d, t) <- parseDateTime
  _ <- comma
  open <- double
  _ <- comma
  hi <- double
  _ <- comma
  lo <- double
  _ <- comma
  close <- double
  pure (FxRow cp d t open hi lo close)
  
  


-- | helpers

space, comma :: Parser Char
space = char ' '
comma = char ','
  
  
