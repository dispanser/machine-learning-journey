{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.ModelSuite where

import           ML.Dataset
import           ML.Model
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

spec_modelSpec :: Spec
spec_modelSpec = do
    let f1 = Continuous "col1" (0, 1)
        f2 = Continuous "col2" (0, 1)
        f3 = Categorical "col3" "blue" ["red", "yellow"]
        fs = createFeatureSpace [f1, f2, f3]

    describe "building model specification from feature names " $ do
        it "should construct a model spec for valid column names" $ do
            let ms  = buildModelSpec fs "col1" ["col2", "col3"]
            response  <$> ms `shouldBe` Right f1
            (knownFeats . features') <$> ms `shouldBe` Right [f2, f3]
        it "should fail when the response variable is missing" $ do
            let ms  = buildModelSpec fs "colX" ["col2", "col3"]
            leftToMaybe ms `shouldBe` Just  "response named 'colX' not found"
        it "should fail when a feature variable is missing" $ do
            let ms  = buildModelSpec fs "col1" ["col2", "colX"]
            leftToMaybe ms `shouldBe` Just  "feature named 'colX' not found"




