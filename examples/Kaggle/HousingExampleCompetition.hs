{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (mapM_)
import           ISL.DataSet.CSV (readCsvWithHeader, writeCsv)
import qualified ISL.Model     as M
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let startColumnNames =
            [ "LotArea", "YearBuilt", "1stFlrSF", "2ndFlrSF", "FullBath"
            , "BedroomAbvGr", "TotRmsAbvGrd"]
    housingDS <- readCsvWithHeader "data/housing/train.csv"
    housingTestDS <- readCsvWithHeader "data/housing/test.csv"
    let Just baseModel = M.extractModelInput "SalePrice" startColumnNames housingDS
        lrFit          = M.fit baseModel :: OLS.LinearRegression
    mapM_ print $ M.summary lrFit

    let Just testData = M.extractFeatureVectors startColumnNames housingTestDS
        prediction    = M.predict lrFit testData
        Just idCol    = M.extractFeatureVector "Id" housingTestDS

    writeCsv "./submission.csv" [idCol, prediction]


