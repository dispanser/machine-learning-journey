{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Data.Text   (Text)
import qualified ISL.DataSet    as DS
import qualified ISL.LinearRegression as OLS

main :: IO ()
main = do
    let startColumnNames =
            [ "LotArea" :: Text, "YearBuilt", "1stFlrSF", "2ndFlrSF", "FullBath"
            , "BedroomAbvGr", "TotRmsAbvGrd", "SalePrice" ]
    housingDS <- DS.readCsvWithHeader "data/housing/train.csv"
    let Just baseModel = DS.extractModelInput "SalePrice" startColumnNames housingDS
    let lrFit     = OLS.linearRegression baseModel
    putStrLn $ DS.summary lrFit

