module Main where

import Lib

-- import Chart.Unit
-- import Chart.Types

import Control.Monad.Catch
import Data.Conduit
import Data.Text
import qualified Data.Conduit.Attoparsec as CA



main = putStrLn "hello!"



asdf :: MonadThrow m => ConduitM Text o m (FxDataSet Double)
asdf = CA.sinkParser parseFxDataset
