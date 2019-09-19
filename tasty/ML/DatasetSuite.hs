{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.DatasetSuite where

import           ML.Dataset
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector.Unboxed as V

fDouble, fNAs, fCat :: Feature
fDouble       = createFeature "col1" ["1.0", "2.0", "3.0"]
fNAs          = createFeature "col2" ["1.0", "NA", "3.0"]
fCat          = createFeature "col3" ["red", "yellow", "blue"]

spec_DatasetFromFeatures :: Spec
spec_DatasetFromFeatures = do
    describe "dataset created with createFromFeatures" $ do
        let Dataset {..} = createFromFeatures "test01" $ [fDouble, fNAs, fCat]
            colRed        = V.fromList [1.0, 0.0, 0.0 ]
            colBlue       = V.fromList [0.0, 0.0, 1.0]
        it "should have proper name" $
            dsName' `shouldBe` "test01"
        it "contain all input features" $ do
            (sortOn (featName' . metadata) dsFeatures) `shouldBe` [fDouble, fNAs, fCat]
        it "contains all columns" $ do
            dsNumCols' `shouldBe` 4
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
            colByName' "col3_green" `shouldBe` Just (V.replicate 3 0.0)
        it "defines the feature space covered" $ do
            (featName' <$> knownFeats featureSpace) `shouldBe` ["col1", "col2", "col3"]

spec_extractDataColumns :: Spec
spec_extractDataColumns = do
    describe "extracting a feature space from a dataset" $ do
        let dataset = createFromFeatures "hspec" [fDouble, fNAs, fCat]
            fs      = featureSpace dataset
        it "should load all found columns" $
            extractDataColumns dataset fs `shouldBe`
                [ V.fromList [1.0, 2.0, 3.0]
                , V.fromList [1.0, 2.0, 3.0]
                , V.fromList [1.0, 0.0, 0.0]
                , V.fromList [0.0, 1.0, 0.0]]
        it "is robust to addition class inhabitants" $ do
            -- we create a dataset with an additional color, green,
            -- to make sure that using the previously defind feature space,
            -- we still only retrieve the columns for red and yellow
            let fGreen   = createFeature "col3" ["red", "yellow", "blue", "green"]
                Just fC3 = (findFeature fs) "col3"
                dataset' = createFromFeatures "hspec" [fGreen]
            featureVectors' dataset' fC3 `shouldBe`
                [ V.fromList [1.0, 0.0, 0.0, 0.0]
                , V.fromList [0.0, 1.0, 0.0, 0.0]]
        it "is robust to removing a class inhabitant" $ do
            -- we create a dataset that's missing a color (yellow)
            -- to make sure that using the previously defind feature space,
            -- we still retrieve the columns for both red and yellow
            let fNoY     = createFeature "col3" ["red", "blue"]
                Just fC3 = (findFeature fs) "col3"
                dataset' = createFromFeatures "hspec" [fNoY]
            featureVectors' dataset' fC3 `shouldBe`
                [ V.fromList [1.0, 0.0]
                , V.fromList [0.0, 0.0] ]
        it "is robust to a new baseline class" $ do
            -- we create a dataset with an additional color, amber,
            -- that is the new baseline (lexicographically smallest)
            -- to make sure that using the previously defind feature space,
            -- we still only retrieve the columns for red and yellow
            let fAmber   = createFeature "col3" ["amber", "yellow", "blue", "amber"]
                Just fC3 = (findFeature fs) "col3"
                dataset' = createFromFeatures "hspec" [fAmber]
            featureVectors' dataset' fC3 `shouldBe`
                [ V.fromList [0.0, 0.0, 0.0, 0.0]
                , V.fromList [0.0, 1.0, 0.0, 0.0]]
        it "somehow doesn't figure out the proper order" $ do
            let fXYZ = createFeature "zyx" ["1.0", "2.0", "3.0"]
                fFCC = createFeature "fcc" ["1.0", "NA", "3.0"]
                fBAC = createFeature "bac" ["red", "yellow", "blue"]
                ds   = createFromFeatures "hspec" [fXYZ, fFCC, fBAC]
                fs   = featureSpace ds
            colByName' ds "zyx" `shouldBe` Just [1.0, 2.0, 3.0]

spec_filterRows :: Spec
spec_filterRows = do
    it "filters to keep even rows" $ do
        filterDataColumn even [1.0, 2.0, 3.0, 4.0, 5.0] `shouldBe` [1.0, 3.0, 5.0::Double]

spec_createFeature :: Spec
spec_createFeature = do
    describe "autodetection feature parser" $ do
        it "handles a continuous variable" $
            createFeature "autodetected" ("1.0" :| ["2.0", "3.0", "4.0", "5.0"]) `shouldBe`
                (Feature (Continuous "autodetected" 0 1) ([[1.0, 2.0, 3.0, 4.0, 5.0]]))
        it "creates categorical feature on non-numerical columns" $ do
            let f = createFeature "col1" ["blue", "red", "yellow"]
            f `shouldBe` Feature (Categorical "col1" "blue" ["red", "yellow"]) [[0.0, 1.0, 0.0 ], [0.0, 0.0, 1.0 ]]

spec_CreateFeature :: Spec
spec_CreateFeature = do
    describe "autodetected feature parser" $ do
        -- it "creates quantitative feature from empty data" $ do
        --     let f = createFeature "col1" []
        --     columns f  `shouldBe` [V.empty]
        --     featName' (metadata f) `shouldBe` "col1"
        it "creates quantitative feature without N/As" $ do
            let f = createFeature "col1" ["1.0", "2.0", "3.0"]
            columns f `shouldBe` [[1, 2, 3]]
            -- f `shouldBe` SingleCol (mkColumn "col1" $ V.fromList )
        it "creates quantitative feature with N/As replaced by variable mean" $ do
            let f = createFeature "col1" ["1.0", "NA", "3.0"]
            columns f `shouldBe` [[1..3]]
    describe "categorical features" $ do
        it "creates categorical feature on non-numerical columns" $ do
            let f = createFeature "col1" ["blue", "red", "yellow"]
            columns f `shouldBe` [ [0.0, 1.0, 0.0 ], [0.0, 0.0, 1.0 ] ]
            (baseLabel $ metadata f)   `shouldBe` "blue"
            (otherLabels $ metadata f) `shouldBe` ["red", "yellow"]
        it "columns and base feature have canonical ordering" $ do
            let f = createFeature "col1" ["red", "yellow", "blue"]
            columns f `shouldBe` [ [1.0, 0.0, 0.0 ], [0.0, 1.0, 0.0 ] ]
            (baseLabel $ metadata f)   `shouldBe` "blue"
            (otherLabels $ metadata f) `shouldBe` ["red", "yellow"]

