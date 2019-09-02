{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.DataSetSuite where

import           ISL.DataSet
import           ISL.Model
import           ISL.DataSet.CSV (readCsvWithHeader)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

spec_CsvDataSet :: Spec
spec_CsvDataSet = do
    describe "reading advertising dataset from csv" $ do
        ds <- runIO $ readCsvWithHeader "data/Advertising.csv"
        it "parses column names" $
            dsColumns ds `shouldBe` ["", "TV", "radio", "newspaper", "sales"]
        it "produces correct number of colums" $
            dsNumCols ds `shouldBe` 5
        it "produces correct number of rows" $
            dsNumRows ds `shouldBe` 200
    describe "reading housing dataset from csv" $ do
        ds <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "reads categorical data" $
            sort . ordNub <$> (colByName ds "MSZoning") `shouldBe` Just (sort ["C (all)", "FV", "RH", "RL", "RM"])

spec_extractFeatures :: Spec
spec_extractFeatures =
    describe "extracting data columns by name from housing data set" $ do
        ds@DataSet { .. } <- runIO $ readCsvWithHeader "data/housing/train.csv"
        it "produces empty result when column does not exist" $
            extractFeatureVector ds "non-existing column" `shouldBe` Nothing
        it "selects the sales price column" $ do
            let salePrice = extractFeatureVector ds "SalePrice"
            V.map showInt <$> featureVector <$> salePrice `shouldBe` V.fromList <$> colByName "SalePrice"
        it "selects multiple columns" $ do
            let Just [msSubClass, yrSold, lotArea] =
                    extractFeatureVectors ds [ "MSSubClass", "YrSold", "LotArea" ]
            Just (V.map showInt $ featureVector msSubClass)  `shouldBe` (V.fromList <$> colByName "MSSubClass")
            Just (V.map showInt $ featureVector yrSold)      `shouldBe` (V.fromList <$> colByName "YrSold")
            Just (V.map showInt $ featureVector lotArea)     `shouldBe` (V.fromList <$> colByName "LotArea")
        it "handles categorical column MSZoning" $ do
            let Just msZoning = extractFeatureVector ds "MSZoning"
            length (featureVectors msZoning) `shouldBe` 4

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
                    [ SingleCol (Column "x1" $ V.fromList [0 .. 10])
                    , SingleCol (Column "x2" $ V.fromList [10, 9 .. 0]) ]
                , miResponse = SingleCol (Column "y" $ V.fromList [0 .. 10])
                , miRows     = 11 }
            (trainModel, testModel) = splitModelInput (cycle [True, False]) model
        it "splits the input model" $ do
            trainModel `shouldBe` ModelInput
                { miName = "hspec_train"
                , miFeatures =
                    [ SingleCol (Column "x1" $ V.fromList [0, 2 .. 10])
                    , SingleCol (Column "x2" $ V.fromList [10, 8 .. 0]) ]
                , miResponse = SingleCol (Column "y" $ V.fromList [0, 2 .. 10])
                , miRows     = 6}
            testModel `shouldBe` ModelInput
                { miName = "hspec_test"
                , miFeatures =
                    [ SingleCol (Column "x1" $ V.fromList [1, 3 .. 9])
                    , SingleCol (Column "x2" $ V.fromList [9, 7 .. 1]) ]
                , miResponse = SingleCol (Column "y" $ V.fromList [1, 3 .. 9])
                , miRows     = 5 }
