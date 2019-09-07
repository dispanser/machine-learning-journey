{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (mapM_)
import           ISL.DataSet.CSV (createDataSet, writeCsv)
import qualified ML.DataSet as DS
import qualified ISL.Model     as M
import qualified ISL.Model.Validation     as MV
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let cols =
            [ "LotArea", "YearBuilt", "1stFlrSF", "2ndFlrSF"
            , "BedroomAbvGr", "TotRmsAbvGrd", "OverallCond"
            , "OverallQual", "LandSlope" , "BldgType", "ExterQual"
            , "MSSubClass", "GarageArea", "BsmtFullBath"
            ]

    housingDS     <- createDataSet "data/housing/train.csv"
    housingTestDS <- createDataSet "data/housing/test.csv"

    let Right modelSpec = M.buildModelSpec
            (DS.featureSpace housingDS) "SalePrice" cols
        lrFit          = M.fit housingDS modelSpec :: OLS.LinearRegression

    mapM_ print $ DS.summary lrFit

    -- let valMSE = MV.validateModel OLS.linearRegression 1 baseModel
    -- putStrLn $ "validation set MSE: " <> show valMSE

    let kFoldMSE = MV.kFoldModel OLS.linearRegression'' 1 5 housingDS modelSpec
    putStrLn $ "5-fold cross validation MSE: " <> show kFoldMSE


    let Just idCol = (DS.colByName' housingTestDS) "Id"
        prediction = DS.featureColumn $ M.predict lrFit housingTestDS

    writeCsv "./submission.csv"
      [ (idCol, show . (round :: Double -> Int))
      , (prediction, show)]


