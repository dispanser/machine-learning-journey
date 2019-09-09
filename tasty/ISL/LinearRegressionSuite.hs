{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ISL.LinearRegressionSuite where

import qualified ML.Model as M
import           ML.Dataset.CSV (createDataset)
import           ML.Dataset (Dataset(..))
import qualified ML.Dataset as DS
import           Control.Monad (zipWithM_)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           ML.LinearRegression (linearRegression', linearRegression'' , LinearRegression (..)
                                      , tStatistics, pValue, fStatistics)
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable (Vector)

spec_ISLRLinearRegression :: Spec
spec_ISLRLinearRegression = parallel $
    describe "Adertising dataset, ISLR chapter 3:" $ do
        advertisingDataset <- runIO $ createDataset "data/Advertising.csv"
        describe "simple linear regression for 'sales ~ TV'" $ do
            let Right modelSpec = M.buildModelSpec
                    (featureSpace advertisingDataset) "sales" ["TV"]
                lr@LinearRegression {..} = linearRegression'
                    advertisingDataset modelSpec
            it "computes coefficients" $
                checkVector lrCoefficients   [7.0325, 0.0475]

            it "computes rse" $
                lrRse `shouldRoughlyEqual` 3.258

            it "computes standard errors" $
                checkVector lrStandardErrors [0.4578, 0.0027]

            it "computes R^2" $
                lrR2 `shouldRoughlyEqual` 0.612

            it "computes t-statistics" $
                checkVector (tStatistics lr) [15.36, 17.667]

            let pValueF = pValue $ fromIntegral lrDF

            it "computes p-values" $
                checkVector (V.map pValueF $ tStatistics lr) [0.0, 0.0]

        describe "multivariate OLS for 'sales ~ TV + Radio + Newsaper'" $ do
            let Right modelSpec = M.buildModelSpec
                    (featureSpace advertisingDataset) "sales" [
                        "TV", "radio", "newspaper"]
                lr@LinearRegression {..} = linearRegression'
                    advertisingDataset modelSpec
            it "computes coeffiicents" $ do
                checkVector lrCoefficients [ 2.939, 0.046, -0.001, 0.189 ]

            it "computes rse" $
                lrRse `shouldRoughlyEqual` 1.686

            it "computes standard errors" $
                checkVector lrStandardErrors [0.311908, 0.001395, 0.005871, 0.008611]

            it "computes F-Statistics" $
                fStatistics lr `shouldRoughlyEqual` 570.271

            it "computes R^2" $
                lrR2 `shouldRoughlyEqual` 0.897

spec_EmptyClassLinearRegression :: Spec
spec_EmptyClassLinearRegression = do
    -- when only training on a subset of the data, it can thusly happen that
    -- some one-hot encoded class vector is 0, leading to a singular matrix
    -- and no solution to the linear equation
    it "should seemlessly handle empty class variables" $ do
        let cat      = DS.createFeature "x" [ "blue", "blue", "blue", "red", "red" ]
            yc       = DS.createFeature "y" [ "1.0", "1.1", "1.2", "0.3", "0.6" ]
            ds       = DS.createFromFeatures "hspec" [cat, yc]
            Right ms = M.buildModelSpec (DS.featureSpace ds) "y" ["x"]
        print ms
        lrCoefficients (linearRegression'' (<= 2) ds ms) `shouldBe` V.fromList [0.0]


shouldRoughlyEqual :: (Show a, Num a, Ord a, Fractional a) => a -> a -> IO ()
shouldRoughlyEqual actual expected = actual `shouldSatisfy` roughlyEqual expected

roughlyEqual :: (Num a, Ord a, Fractional a) => a -> a -> Bool
roughlyEqual expected actual = 0.001 > abs (expected - actual)

checkVector :: Vector Double -> [Double] -> IO ()
checkVector xs y = do
    V.length xs `shouldBe` length y
    zipWithM_ (\x ex -> x `shouldRoughlyEqual` ex) (V.toList xs) y

