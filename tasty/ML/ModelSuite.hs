{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.ModelSuite where

import           ML.DataSet
import           ISL.Model
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

spec_modelSpec :: Spec
spec_modelSpec = do
    let f1 = FeatureSpec "col1" "col1" []
        f2 = FeatureSpec "col2" "col2" []
        f3 = FeatureSpec "col3" "blue" ["red", "yellow"]
        fs = createFeatureSpace [f1, f2, f3]

    describe "building model specification from feature names " $ do
        it "should construct a model spec for valid column names" $ do
            let ms  = modelSpecByFeatureNames fs "col1" ["col2", "col3"]
            response  <$> ms `shouldBe` Right f1
            (knownFeats . features') <$> ms `shouldBe` Right ["col2", "col3"]
        it "should fail when the response variable is missing" $ do
            let ms  = modelSpecByFeatureNames fs "colX" ["col2", "col3"]
            leftToMaybe ms `shouldBe` Just  "response named 'colX' not found"
        it "should fail when a feature variable is missing" $ do
            let ms  = modelSpecByFeatureNames fs "col1" ["col2", "colX"]
            leftToMaybe ms `shouldBe` Just  "feature named 'colX' not found"




