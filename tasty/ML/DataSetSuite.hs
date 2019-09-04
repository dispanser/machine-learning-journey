{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.DataSetSuite where

import           ML.DataSet
import           ISL.Model
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

spec_DataSetFromFeatures :: Spec
spec_DataSetFromFeatures = do
    describe "dataset created with createFromFeatures" $ do
        let fDouble       = createFeature "col1" ["1.0", "2.0", "3.0"]
            fNAs          = createFeature "col2" ["1.0", "NA", "3.0"]
            fCat          = createFeature "col3" ["red", "yellow", "blue"]
            DataSet' {..} = createFromFeatures "test01" $ [fDouble, fNAs, fCat]
        it "should have proper name" $
            dsName' `shouldBe` "test01"
        it "contain all input features" $ do
            dsFeatures `shouldSatisfy` (fDouble `elem`)
            dsFeatures `shouldSatisfy` (fNAs    `elem`)
            dsFeatures `shouldSatisfy` (fCat    `elem`)
        it "contains all columns" $ do
            dsColumns' `shouldSatisfy` (elem $ Column "col3_red" $ V.fromList [1.0, 0.0, 0.0 ])
            length dsColumns' `shouldBe` 4
        it "nows the proper number of rows" $
            dsNumRows' `shouldBe` 3

spec_CreateFeature :: Spec
spec_CreateFeature = do
    it "creates quantitative feature from empty data" $ do
        createFeature "col1" [] `shouldBe` SingleCol (Column "col1" V.empty)
    it "creates quantitative feature without N/As" $ do
        let f = createFeature "col1" ["1.0", "2.0", "3.0"]
        f `shouldBe` SingleCol (Column "col1" $ V.fromList [1..3])
    it "creates quantitative feature with N/As replaced by variable mean" $ do
        let f = createFeature "col1" ["1.0", "NA", "3.0"]
        f `shouldBe` SingleCol (Column "col1" $ V.fromList [1..3])
    describe "categorical features" $ do
        it "creates categorical feature on non-numerical columns" $ do
            let f = createFeature "col1" ["blue", "red", "yellow"]
            f `shouldBe` MultiCol (Categorical "col1" "blue" $
                [ Column "col1_red"    $ V.fromList [0.0, 1.0, 0.0 ]
                , Column "col1_yellow" $ V.fromList [0.0, 0.0, 1.0 ] ])
        it "columns and base feature are canonically ordered" $ do
            let f = createFeature "col1" ["red", "yellow", "blue"]
            f `shouldBe` MultiCol (Categorical "col1" "blue" $
                [ Column "col1_red"    $ V.fromList [1.0, 0.0, 0.0 ]
                , Column "col1_yellow" $ V.fromList [0.0, 1.0, 0.0 ] ])

