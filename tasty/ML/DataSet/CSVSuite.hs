{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.DataSet.CSVSuite where


import           ISL.DataSet.CSV (createDataSet, createDataSet')
import           ML.DataSet (DataSet'(..), featureName)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

spec_createDataSet :: Spec
spec_createDataSet = do
    describe "createDataSet" $ do
        DataSet' {..} <- runIO $ createDataSet "data/categorical.csv"
        it "reads all features from the csv" $ do
            (featureName <$> dsFeatures) `shouldBe` ["cat1", "cat2"]
    describe "createDataSet'" $ do
        DataSet' {..} <- runIO $ createDataSet' (=="cat2") "data/categorical.csv"
        it "reads all features from the csv" $ do
            (featureName <$> dsFeatures) `shouldBe` ["cat2"]




