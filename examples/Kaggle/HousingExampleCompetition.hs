{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (mapM_)
import           ISL.DataSet.CSV (readCsvWithHeader, writeCsv)
import           ISL.DataSet (summary)
import qualified ISL.Model     as M
import qualified ISL.Model.Validation as MV
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
    mapM_ print $ summary lrFit

    let valMSE = MV.validateModel OLS.linearRegression 1 baseModel
    putStrLn $ "validation set MSE: " <> show valMSE

    let kFoldMSE = MV.kFoldModel OLS.linearRegression 1 5 baseModel
    putStrLn $ "5-fold cross validation MSE: " <> show kFoldMSE

    let Just testData = M.extractFeatureVectors startColumnNames housingTestDS
        prediction    = M.predict lrFit testData
        Just idCol    = M.extractFeatureVector "Id" housingTestDS

    writeCsv "./submission.csv" [idCol, prediction]


