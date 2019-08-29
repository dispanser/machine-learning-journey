{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (forM_)
import qualified Data.Text as T
import           ISL.DataSet.CSV (readCsvWithHeader, writeCsv)
import qualified ISL.Model     as M
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let startColumnNames =
            [ "LotArea" :: T.Text, "YearBuilt", "1stFlrSF", "2ndFlrSF", "FullBath"
            , "BedroomAbvGr", "TotRmsAbvGrd"]
    housingDS <- readCsvWithHeader "data/housing/train.csv"
    housingTestDS <- readCsvWithHeader "data/housing/test.csv"
    let Just baseModel = M.extractModelInput "SalePrice" startColumnNames housingDS
        lrFit          = M.fit baseModel :: OLS.LinearRegression
    forM_ (T.lines $ M.summary lrFit) print
    let Just testData = M.extractFeatureVectors startColumnNames housingTestDS
        prediction    = M.predict lrFit testData
        Just idCol    = M.extractFeatureVector "Id" housingTestDS


    writeCsv "./submission.csv" [idCol, prediction]


