{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.DataSuite where

import           ISL.Data
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

spec_CsvDataSet :: Spec
spec_CsvDataSet =
    describe "reading advertising dataset from csv" $ do
        DataSet { .. } <- runIO $ readCsvWithHeader "data/Advertising.csv"
        it "parses column names" $
            columnNames `shouldBe` V.fromList ["", "TV", "radio", "newspaper", "sales"]
        it "produces correct number of colums" $
            length columnData `shouldBe` 5
        it "produces correct number of rows" $
            V.length (head columnData) `shouldBe` 200

spec_extractFeatures :: Spec
spec_extractFeatures =
    describe "extracting data columns by name from housing data set" $ do
        ds@DataSet { .. } <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "produces empty result when column does not exist" $
            extractFeatureVector "non-existing column" ds `shouldBe` Nothing
        it "selects the sales price column" $ do
            let salePrice = extractFeatureVector "SalePrice" ds
            salePrice `shouldBe` Just (columnData !! 80)
        it "selects multiple columns" $ do
            let Just [msSubClass, yrSold, lotArea] =
                    extractFeatureVectors [ "MSSubClass", "YrSold", "LotArea" ] ds
            msSubClass  `shouldBe` (columnData !! 1)
            yrSold      `shouldBe` (columnData !! 77)
            lotArea     `shouldBe` (columnData !! 4)

spec_extractDataSet :: Spec
spec_extractDataSet =
    describe "extracting a data set from another data set by column names" $ do
        dsFull <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "creates a correct dataset" $ do
            let Just dsSub = extractDataSet [ "MSSubClass", "YrSold", "LotArea" ] dsFull
            name dsSub `shouldBe` name dsFull

