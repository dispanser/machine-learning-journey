{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.Data.FeatureSuite where

import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           ML.Data.Feature.Internal

spec_CreateFeature :: Spec
spec_CreateFeature = do
    describe "autodetected feature parser" $ do
        it "creates quantitative feature without N/As" $ do
            let f = createFeature "col1" ["1.0", "2.0", "3.0"]
            columns f `shouldBe` [[1..3]]
            f `shouldBe` (Feature (Continuous "col1" (0, 1)) ([[1..3]]))
        it "creates quantitative feature with N/As replaced by variable mean" $ do
            let f = createFeature "col1" ["1.0", "NA", "3.0"]
            f `shouldBe` (Feature (Continuous "col1" (0, 1)) ([[1..3]]))
        it "creates categorical feature on non-numerical columns" $ do
            let f = createFeature (auto "col1") ["blue", "red", "yellow"]
            f `shouldBe` Feature (Categorical "col1" "blue" ["red", "yellow"]) [[0.0, 1.0, 0.0 ], [0.0, 0.0, 1.0 ]]
            (baseLabel $ metadata f)   `shouldBe` "blue"
    describe "categorical features" $ do
        it "creates categorical feature on non-numerical columns" $ do
            let f = createFeature "col1" ["blue", "red", "yellow"]
            columns f `shouldBe` [ [0.0, 1.0, 0.0 ], [0.0, 0.0, 1.0 ] ]
            (otherLabels $ metadata f) `shouldBe` ["red", "yellow"]
        it "columns and base feature have canonical ordering" $ do
            let f = createFeature "col1" ["red", "yellow", "blue"]
            columns f `shouldBe` [ [1.0, 0.0, 0.0 ], [0.0, 1.0, 0.0 ] ]
            (baseLabel $ metadata f)   `shouldBe` "blue"
            (otherLabels $ metadata f) `shouldBe` ["red", "yellow"]
    describe "scaling a continuous feature" $ do
        it "using 0,1 min-max scaling" $ do
            let f = createFeature (scaled01 "scaled col") ["1", "2", "3", "4", "5"]
            columns f `shouldBe` [[0,0.25..1]]
        it "scaling explicitely turned off" $ do
            let f = createFeature (scaled noScaling "unscaled col") ["1", "2", "3", "4", "5"]
            columns f `shouldBe` [[1..5]]

