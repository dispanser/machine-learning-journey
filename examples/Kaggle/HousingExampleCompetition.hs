{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Control.Monad (mapM_)
import           ML.Dataset.CSV (writeCsv, readRawData)
import qualified ML.Dataset as DS
import qualified ML.Model     as M
import qualified ML.Model.Validation     as MV
import qualified ML.LinearRegression as OLS
import           ML.Data.Summary (summary)

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
            , "RoofStyle"
            ]

    Right housingDS     <- DS.parseFullDataset <$> readRawData "data/housing/train.csv"
    Right housingTestDS <- DS.parseFullDataset <$> readRawData "data/housing/test.csv"

    let Right ms  = M.buildModelSpec (DS.featureSpace housingDS) "SalePrice" cols
        lrModel   = OLS.fitLinearRegression ms
        lrFit     = M.fitDataset lrModel housingDS

    mapM_ print $ summary lrFit

    let valMSE = MV.validateModel lrModel 1 housingDS
    putStrLn $ "validation set MSE: " <> show valMSE

    let kFoldMSE = MV.kFoldModel lrModel 1 5 housingDS
    putStrLn $ "5-fold cross validation MSE: " <> show kFoldMSE

    let Just idCol = (DS.colByName' housingTestDS) "Id"
        prediction = DS.featureColumn $ M.predictDataset lrFit housingTestDS

    writeCsv "./submission.csv"
      [ (idCol, show . (round :: Double -> Int))
      , (prediction, show)]


