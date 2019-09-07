{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.DataSetSuite where

import           ML.DataSet
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

fDouble, fNAs, fCat :: Feature Double
fDouble       = createFeature "col1" ["1.0", "2.0", "3.0"]
fNAs          = createFeature "col2" ["1.0", "NA", "3.0"]
fCat          = createFeature "col3" ["red", "yellow", "blue"]

spec_DataSetFromFeatures :: Spec
spec_DataSetFromFeatures = do
    describe "dataset created with createFromFeatures" $ do
        let DataSet' {..} = createFromFeatures "test01" $ [fDouble, fNAs, fCat]
            colRed        = Column "col3_red" $ V.fromList [1.0, 0.0, 0.0 ]
            colBlue       = Column "col3_blue" $ V.fromList [0.0, 0.0, 1.0]
        it "should have proper name" $
            dsName' `shouldBe` "test01"
        it "contain all input features" $ do
            dsFeatures `shouldSatisfy` (fDouble `elem`)
            dsFeatures `shouldSatisfy` (fNAs    `elem`)
            dsFeatures `shouldSatisfy` (fCat    `elem`)
        it "contains all columns" $ do
            dsColumns' `shouldSatisfy` (elem colRed)
            length dsColumns' `shouldBe` 4
        it "nows the proper number of rows" $
            dsNumRows' `shouldBe` 3
        it "supports lookup by column name" $ do
            colByName' "col3_red" `shouldBe` Just colRed
        it "supports lookup by feature name" $ do
            featByName "col2" `shouldBe` Just fNAs
        it "supports lookup by column name of baseline class" $
            colByName' "col3_blue" `shouldBe` Just colBlue
        it "supports lookup by column name of non-existing class" $
            -- useful when the test set contains previously unseen classes,
            -- or the other way around: just return an empty column, given
            -- that at least the underlying feature exists and is categorical.
            colByName' "col3_green" `shouldBe` Just (Column "col3_green" $ V.replicate 3 0.0)
        it "defines the feature space covered" $ do
            (featName <$> knownFeats featureSpace) `shouldBe` ["col1", "col2", "col3"]

spec_extractDataColumns :: Spec
spec_extractDataColumns = do
    it "extracting a feature space from a dataset" $ do
        let dataset = createFromFeatures "hspec" [fDouble, fNAs, fCat]
            fs      = featureSpace dataset
        extractDataColumns dataset fs `shouldBe`
            [ Column "col1" $ V.fromList [1.0, 2.0, 3.0]
            , Column "col2" $ V.fromList [1.0, 2.0, 3.0]
            , Column "col3_red" $  V.fromList [1.0, 0.0, 0.0]
            , Column "col3_yellow" $ V.fromList [0.0, 1.0, 0.0]]

spec_filterRows :: Spec
spec_filterRows = do
    it "filters to keep even rows" $ do
        let c = Column "col1" $ V.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
        filterDataColumn even c `shouldBe`
            (Column "col1" $ V.fromList [1.0, 3.0, 5.0 :: Double])

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

