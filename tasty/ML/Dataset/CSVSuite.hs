{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.Dataset.CSVSuite where


import           ISL.Dataset.CSV (createDataset, createDataset')
import           ML.Dataset (Dataset(..), featureName)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

spec_createDataset :: Spec
spec_createDataset = do
    describe "createDataset" $ do
        Dataset {..} <- runIO $ createDataset "data/categorical.csv"
        it "reads all features from the csv" $ do
            (featureName <$> dsFeatures) `shouldBe` ["cat1", "cat2"]
    describe "createDataset'" $ do
        Dataset {..} <- runIO $ createDataset' (=="cat2") "data/categorical.csv"
        it "reads all features from the csv" $ do
            (featureName <$> dsFeatures) `shouldBe` ["cat2"]




