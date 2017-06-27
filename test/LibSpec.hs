{-# LANGUAGE OverloadedStrings #-}
module LibSpec where

import Data.Text
import Data.Attoparsec.Text (parseOnly)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Attoparsec

import Lib


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib.Parsers.Forex" $ do
    it "parses a row of data from fxhistoricaldata.com correctly" $ do
      let
        row0 :: Text
        row0 = "GBPJPY,2017-06-26 21:00:00,142.3470,142.3520,142.2970,142.3140"
      row0 ~> (rateClose <$> parseFxRow) `shouldParse` 142.3140
--     -- prop "ourAdd is commutative" $ \x y ->
--     --   ourAdd x y `shouldBe` ourAdd y x





