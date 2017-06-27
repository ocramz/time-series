{-# language OverloadedStrings #-}
module Lib.Parsers.Forex (Currency, CurrencyPair(..), FxRow(..), parseFxRow, parseFxDataset) where

import Data.TimeSeries.Forex.Types

import Control.Applicative ((<|>))

import qualified Data.Attoparsec.Internal.Types as A
import Data.Attoparsec.Text hiding (space)
import Data.Text
import Data.Time (Day, TimeOfDay)
import Attoparsec.Time



parseCurrencyPair :: A.Parser Text CurrencyPair
parseCurrencyPair =
  (string "GBPJPY" *> pure (CPair GBP JPY) ) <|>
  (string "EURUSD" *> pure (CPair EUR USD) ) <|>
  (string "AUDJPY" *> pure (CPair AUD JPY) ) <|>
  (string "AUDUSD" *> pure (CPair AUD USD) ) <|>
  (string "EURCHF" *> pure (CPair EUR CHF) ) <|>
  (string "EURGBP" *> pure (CPair EUR GBP) ) <|>
  (string "EURJPY" *> pure (CPair EUR JPY) ) <|>
  (string "GBPUSD" *> pure (CPair GBP USD) ) <|>
  (string "USDJPY" *> pure (CPair USD JPY) )  
  
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
  


parseFxDataset :: A.Parser Text [FxRow Double]
parseFxDataset = sepBy parseFxRow endOfLine <* endOfInput
  


-- | helpers

space, comma :: Parser Char
space = char ' '
comma = char ','
  
  
