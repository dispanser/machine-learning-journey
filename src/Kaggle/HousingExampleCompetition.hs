{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module Kaggle.HousingExampleCompetition where

import           Data.Text   (Text)
import           Data.Vector.Storable (convert)
import           ISL.Data    (DataSet(..))
import qualified ISL.Data    as DS
import qualified ISL.LinearRegression as OLS

runExample :: IO ()
runExample = do
    let startColumnNames =
            [ "LotArea" :: Text, "YearBuilt", "1stFlrSF", "2ndFlrSF", "FullBath"
            , "BedroomAbvGr", "TotRmsAbvGrd", "SalePrice" ]
    fullHousingDS <- DS.readCsvWithHeader "data/housing/train.csv"
    let Just myHousingDS@DataSet {..} = DS.extractModelInput startColumnNames fullHousingDS
    let Just xX = DS.extractFeatureVectors (init startColumnNames) myHousingDS
    let Just y  = DS.extractFeatureVector "SalePrice" myHousingDS
    let lrFit   = OLS.linearRegression (convert <$> xX) (convert y)
    putStrLn $ DS.summary lrFit


