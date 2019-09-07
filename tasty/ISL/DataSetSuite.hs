{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.DataSetSuite where

import           ML.DataSet
import           ISL.Model
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

spec_replaceNAs :: Spec
spec_replaceNAs =
    it "N/A values should be replaced by the variable mean" $ do
        let vs = V.fromList [-1, sqrt $ -1, 3]
        replaceNAs vs `shouldBe` V.fromList [-1, 1, 3 :: Double]

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
