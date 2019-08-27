{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.DataSuite where

import           ISL.DataSet
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

spec_CsvDataSet :: Spec
spec_CsvDataSet =
    describe "reading advertising dataset from csv" $ do
        DataSet { .. } <- runIO $ readCsvWithHeader "data/Advertising.csv"
        it "parses column names" $
            dsColumnNames `shouldBe` V.fromList ["", "TV", "radio", "newspaper", "sales"]
        it "produces correct number of colums" $
            length dsColumnData `shouldBe` 5
        it "produces correct number of rows" $
            V.length (head dsColumnData) `shouldBe` 200

spec_extractFeatures :: Spec
spec_extractFeatures =
    describe "extracting data columns by name from housing data set" $ do
        ds@DataSet { .. } <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "produces empty result when column does not exist" $
            extractFeatureVector "non-existing column" ds `shouldBe` Nothing
        it "selects the sales price column" $ do
            let salePrice = extractFeatureVector "SalePrice" ds
            salePrice `shouldBe` Just (dsColumnData !! 80)
        it "selects multiple columns" $ do
            let Just [msSubClass, yrSold, lotArea] =
                    extractFeatureVectors [ "MSSubClass", "YrSold", "LotArea" ] ds
            msSubClass  `shouldBe` (dsColumnData !! 1)
            yrSold      `shouldBe` (dsColumnData !! 77)
            lotArea     `shouldBe` (dsColumnData !! 4)

spec_extractDataSet :: Spec
spec_extractDataSet =
    describe "extracting a data set from another data set by column names" $ do
        dsFull <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "creates a correct dataset" $ do
            let Just dsSub = extractModelInput [ "MSSubClass", "YrSold", "LotArea" ] dsFull
            dsName dsSub `shouldBe` dsName dsFull

