{-# language OverloadedStrings #-}
module Lib.Parsers.Forex (Currency, CurrencyPair(..), FxRow(..), parseFxRow, parseFxDataset) where

import Data.TimeSeries.Forex.Types

import Control.Applicative ((<|>))

import qualified Data.Attoparsec.Internal.Types as A
import Data.Attoparsec.Text hiding (space)
import Data.Text
import Data.Time (Day, TimeOfDay)
import qualified Attoparsec.Time as AT



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
  d <- AT.dayInISO8601
  _ <- space
  t <- AT.timeOfDayInISO8601
  return (d, t)

parseFxRow0 :: A.Parser Text (FxRow Double)
parseFxRow0 = do
  (d, t) <- parseDateTime
  _ <- comma
  open <- double
  _ <- comma
  hi <- double
  _ <- comma
  lo <- double
  _ <- comma
  close <- double
  pure $ FxRow d t open hi lo close

-- | parse a whole row, discarding the repeated currency pair annotation
parseFxRow :: A.Parser Text (FxRow Double)
parseFxRow = many1 letter *> comma *> parseFxRow0
  
-- | Parse the whole dataset
parseFxDataset :: A.Parser Text (FxDataSet Double)
parseFxDataset = do
  curr <- parseCurrencyPair <* comma
  r1 <- parseFxRow0 <* endOfLine
  rs <- sepBy parseFxRow endOfLine -- <* endOfInput
  pure $ FxDataset curr rs
  

  


-- | helpers

letters :: Char -> Bool
letters = inClass "a-zA-Z"

space, comma :: Parser Char
space = char ' '
comma = char ','
  
  


t0, t1 :: Text
t0 = "GBPJPY,2017-06-26 21:00:00,142.3470,142.3520,142.2970,142.3140"
t1 = "2017-06-26 21:00:00,142.3470,142.3520,142.2970,142.3140"
