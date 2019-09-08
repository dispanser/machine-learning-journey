{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ISL.LinearRegressionSuite where

import           ISL.Model       (buildModelSpec)
import           ISL.Dataset.CSV (createDataset)
import           ML.Dataset (Dataset(..))
import           Control.Monad (zipWithM_)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           ISL.LinearRegression (linearRegression' , LinearRegression (..)
                                      , tStatistics, pValue, fStatistics)
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable (Vector)

spec_ISLRLinearRegression' :: Spec
spec_ISLRLinearRegression' = parallel $
    describe "Adertising dataset, ISLR chapter 3:" $ do
        advertisingDataset <- runIO $ createDataset "data/Advertising.csv"
        describe "simple linear regression for 'sales ~ TV'" $ do
            let Right modelSpec = buildModelSpec
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
            let Right modelSpec = buildModelSpec
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

shouldRoughlyEqual :: (Show a, Num a, Ord a, Fractional a) => a -> a -> IO ()
shouldRoughlyEqual actual expected = actual `shouldSatisfy` roughlyEqual expected

roughlyEqual :: (Num a, Ord a, Fractional a) => a -> a -> Bool
roughlyEqual expected actual = 0.001 > abs (expected - actual)

checkVector :: Vector Double -> [Double] -> IO ()
checkVector xs y = do
    V.length xs `shouldBe` length y
    zipWithM_ (\x ex -> x `shouldRoughlyEqual` ex) (V.toList xs) y

