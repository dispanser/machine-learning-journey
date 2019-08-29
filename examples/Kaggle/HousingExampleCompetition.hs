{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (forM_)
import qualified Data.Text as T
import qualified ISL.DataSet     as DS
import           ISL.DataSet.CSV (readCsvWithHeader, writeCsv)
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let startColumnNames =
            [ "LotArea" :: T.Text, "YearBuilt", "1stFlrSF", "2ndFlrSF", "FullBath"
            , "BedroomAbvGr", "TotRmsAbvGrd"]
    housingDS <- readCsvWithHeader "data/housing/train.csv"
    housingTestDS <- readCsvWithHeader "data/housing/test.csv"
    let Just baseModel = DS.extractModelInput "SalePrice" startColumnNames housingDS
        lrFit          = DS.fit baseModel :: OLS.LinearRegression
    forM_ (T.lines $ DS.summary lrFit) print
    let Just testData = DS.extractFeatureVectors startColumnNames housingTestDS
        prediction    = DS.predict lrFit testData
        Just idCol    = DS.extractFeatureVector "Id" housingTestDS


    writeCsv "./submission.csv" [idCol, prediction]


