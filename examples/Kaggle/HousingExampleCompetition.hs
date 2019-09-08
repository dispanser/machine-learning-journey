{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (mapM_)
import           ML.Dataset.CSV (createDataset, writeCsv)
import qualified ML.Dataset as DS
import qualified ML.Model     as M
import qualified ML.Model.Validation     as MV
import qualified ML.LinearRegression as OLS

main :: IO ()
main = do
    let cols =
            [ "LotArea", "YearBuilt", "1stFlrSF", "2ndFlrSF"
            , "BedroomAbvGr", "TotRmsAbvGrd", "OverallCond"
            , "OverallQual", "LandSlope" , "BldgType", "ExterQual"
            , "MSSubClass", "GarageArea", "BsmtFullBath"
            , "YrSold", "Fireplaces", "FullBath"
            , "BsmtHalfBath", "LotConfig", "LotShape", "MasVnrArea"
            , "ExterQual", "KitchenAbvGr", "GarageCars"
            , "ScreenPorch", "GarageFinish", "KitchenQual"
            ]

    housingDS     <- createDataset "data/housing/train.csv"
    housingTestDS <- createDataset "data/housing/test.csv"

    let Right modelSpec = M.buildModelSpec
            (DS.featureSpace housingDS) "SalePrice" cols
        lrFit          = M.fit housingDS modelSpec :: OLS.LinearRegression

    mapM_ print $ DS.summary lrFit

    let valMSE = MV.validateModel OLS.linearRegression'' 1 housingDS modelSpec
    putStrLn $ "validation set MSE: " <> show valMSE

    let kFoldMSE = MV.kFoldModel OLS.linearRegression'' 1 5 housingDS modelSpec
    putStrLn $ "5-fold cross validation MSE: " <> show kFoldMSE

    let Just idCol = (DS.colByName' housingTestDS) "Id"
        prediction = DS.featureColumn $ M.predict lrFit housingTestDS

    writeCsv "./submission.csv"
      [ (idCol, show . (round :: Double -> Int))
      , (prediction, show)]


