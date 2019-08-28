{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (forM_)
import qualified Data.Text as T
import qualified ISL.DataSet    as DS
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let startColumnNames =
            [ "LotArea" :: T.Text, "YearBuilt", "1stFlrSF", "2ndFlrSF", "FullBath"
            , "BedroomAbvGr", "TotRmsAbvGrd"]
    housingDS <- DS.readCsvWithHeader "data/housing/train.csv"
    _housingTestDS <- DS.readCsvWithHeader "data/housing/test.csv"
    let Just baseModel = DS.extractModelInput "SalePrice" startColumnNames housingDS
        lrFit          = OLS.linearRegression baseModel
    forM_ (T.lines $ DS.summary lrFit) print

