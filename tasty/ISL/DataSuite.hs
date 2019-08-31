{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.DataSuite where

import           ISL.DataSet
import           ISL.Model
import           ISL.DataSet.CSV (readCsvWithHeader)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

spec_CsvDataSet :: Spec
spec_CsvDataSet =
    describe "reading advertising dataset from csv" $ do
        ds@DataSet { .. } <- runIO $ readCsvWithHeader "data/Advertising.csv"
        it "parses column names" $
            M.keys dsColumnIndices `shouldBe` sort ["", "TV", "radio", "newspaper", "sales"]
        it "produces correct number of colums" $
            numCols ds `shouldBe` 5
        it "produces correct number of rows" $
            numRows ds `shouldBe` 200

spec_extractFeatures :: Spec
spec_extractFeatures =
    describe "extracting data columns by name from housing data set" $ do
        ds@DataSet { .. } <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "produces empty result when column does not exist" $
            extractFeatureVector "non-existing column" ds `shouldBe` Nothing
        it "selects the sales price column" $ do
            let salePrice = extractFeatureVector "SalePrice" ds
            V.map showInt <$> colData <$> salePrice `shouldBe` M.lookup "SalePrice" dsColumnIndices
        it "selects multiple columns" $ do
            let Just [msSubClass, yrSold, lotArea] =
                    extractFeatureVectors [ "MSSubClass", "YrSold", "LotArea" ] ds
            Just (V.map showInt $ colData msSubClass)  `shouldBe` (M.lookup "MSSubClass" dsColumnIndices)
            Just (V.map showInt $ colData yrSold)      `shouldBe` (M.lookup "YrSold" dsColumnIndices)
            Just (V.map showInt $ colData lotArea)     `shouldBe` (M.lookup "LotArea" dsColumnIndices)
        it "handles categorical column MSZoning" $ do
            let Just msZoning = extractFeatureVector "MSZoning" ds
            13 `shouldBe` 14
        where
          showInt :: Double -> Text
          showInt = show . (round :: Double -> Int)


spec_extractDataSet :: Spec
spec_extractDataSet =
    describe "create model from data set using column names" $ do
        dsFull <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "creates a correct dataset" $ do
            let Just modelInput = extractModelInput "SalePrice" [ "MSSubClass", "YrSold", "LotArea" ] dsFull
            miName modelInput `shouldBe` dsName dsFull

spec_splitVector :: Spec
spec_splitVector =
    describe "splitting a vector into two" $ do
        it "should handle empty vectors" $ do
            let (left, right) = splitVector (repeat False) (V.empty :: V.Vector Bool)
            left `shouldBe` V.empty
            right `shouldBe` V.empty
        it "should handle empty row selector" $ do
            let inp           = V.fromList [3, 4, 5, 6 :: Int]
                (left, right) = splitVector (repeat False) inp
            left `shouldBe` V.empty
            right `shouldBe` inp
        it "should handle full row selector" $ do
            let inp           = V.fromList [3 .. 7 :: Int]
                (left, right) = splitVector (repeat True) inp
            left `shouldBe` inp
            right `shouldBe` V.empty

spec_splitModel :: Spec
spec_splitModel =
    describe "splitting a model input" $ do
        let model = ModelInput
                { miName = "hspec"
                , miFeatures =
                    [ Column "x1" $ V.fromList [0 .. 10]
                    , Column "x2" $ V.fromList [10, 9 .. 0] ]
                , miResponse = Column "y" $ V.fromList [0 .. 10] }
            (trainModel, testModel) = splitModelInput (cycle [True, False]) model
        it "splits the input model" $ do
            trainModel `shouldBe` ModelInput
                { miName = "hspec_train"
                , miFeatures =
                    [ Column "x1" $ V.fromList [0, 2 .. 10]
                    , Column "x2" $ V.fromList [10, 8 .. 0] ]
                , miResponse = Column "y" $ V.fromList [0, 2 .. 10] }
            testModel `shouldBe` ModelInput
                { miName = "hspec_test"
                , miFeatures =
                    [ Column "x1" $ V.fromList [1, 3 .. 9]
                    , Column "x2" $ V.fromList [9, 7 .. 1] ]
                , miResponse = Column "y" $ V.fromList [1, 3 .. 9] }
