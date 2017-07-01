{-# language FlexibleContexts #-}
module Main where

import Lib

-- import Chart.Unit
-- import Chart.Types
-- import Control.Monad (forM, forM_)
-- import Control.Monad.State.Class
import Control.Monad.Catch

import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA

-- import Data.Text
import qualified Data.Text.IO as T

import Data.Time

-- import System.Environment (getArgs, withArgs)

import qualified Data.Attoparsec.Text as A

import Control.Arrow ((&&&))

import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine (mainWith)


fname :: String
fname = "data/forex/GBPJPY_hour.csv"


main :: IO ()
main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right (FxDataset _ datarows) ->
               mainWith $
                 timeSeriesPlots ds where
                   npoints = 500  -- # of ticks to plot
                   fc = 138  -- mean correction
                   ds = [
                     -- (data_open, green),
                     (data_hi, red),
                     (data_lo, blue),
                     (data_minmaxdiff, black)
                     -- (data_close, black)
                     ]
                   -- data_open = take npoints $ open <$> datarows
                   data_hi = take npoints $ high <$> datarows
                   data_lo = take npoints $ low <$> datarows
                   -- data_close = take npoints $ close <$> datarows
                   data_minmaxdiff = take npoints $ zipWithTS fmm rateHigh rateLow (dateTimeToNum <$> date <*> timeOfDay) datarows where
                     fmm x y = fc + x - y




-- asdf :: MonadThrow m => ConduitM Text o m (FxDataSet Double)
-- asdf = CA.sinkParser parseFxDataset





-- * Helpers

zipWithTS :: (a -> b -> c)
     -> (x -> a) -> (x -> b) -> (x -> t) -> [x] -> [(t, c)]
zipWithTS fzip fx fy ft ts = zip t_ (zipWith fzip x y) where
  x = fx  <$> ts
  y = fy  <$> ts
  t_ = ft <$> ts
