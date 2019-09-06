{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (mapM_)
import           ISL.DataSet.CSV (readCsvWithHeader, writeCsv)
import           ML.DataSet (summary)
import qualified ISL.Model     as M
import qualified ISL.Model.Validation as MV
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let cols =
            [ "LotArea", "YearBuilt", "1stFlrSF", "2ndFlrSF"
            , "BedroomAbvGr", "TotRmsAbvGrd", "OverallCond"
            , "OverallQual", "LandSlope" , "BldgType", "ExterQual"
            , "MSSubClass", "GarageArea", "BsmtFullBath"
            ]
    housingDS <- readCsvWithHeader "data/housing/train.csv"
    housingTestDS <- readCsvWithHeader "data/housing/test.csv"
    let Just baseModel = M.extractModelInput "SalePrice" cols housingDS
        lrFit          = M.fit baseModel :: OLS.LinearRegression
    mapM_ print $ summary lrFit

    let valMSE = MV.validateModel OLS.linearRegression 1 baseModel
    putStrLn $ "validation set MSE: " <> show valMSE

    let kFoldMSE = MV.kFoldModel OLS.linearRegression 1 5 baseModel
    putStrLn $ "5-fold cross validation MSE: " <> show kFoldMSE

    let Just testData = M.extractFeatureVectors housingTestDS cols
        prediction    = M.predict lrFit testData
        Just idCol    = M.extractFeatureVector housingTestDS "Id"

    writeCsv "./submission.csv" [idCol, prediction]


