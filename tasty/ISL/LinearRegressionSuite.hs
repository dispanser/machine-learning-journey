{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ISL.LinearRegressionSuite where

import qualified ML.Model as M
import           ML.Dataset.CSV (createDataset)
import           ML.Dataset (Dataset(..))
import qualified ML.Dataset as DS
import           Control.Monad (zipWithM_)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           ML.LinearRegression (linearRegression', linearRegression''
                                     , LinearRegression (..)
                                     , tStatistics, pValue, fStatistics)
import qualified ML.LinearRegressionGD as LRGD
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Unboxed as VU

type LR a = DS.Column Double -> [DS.Column Double] -> M.ModelSpec -> a

spec_babyRegression :: Spec
spec_babyRegression = do
    describe "simple linear regression" $ do
        -- perfect line: y = 2x-1
        let x  = DS.SingleCol $ DS.Column "x" $ VU.fromList [2.0, 3.0, 4.0 :: Double]
            y  = DS.SingleCol $ DS.Column "y" $ VU.fromList [3.0, 5.0, 7.0 :: Double]
            ds = DS.createFromFeatures "hspec" [x, y]
            Right ms = M.buildModelSpec (DS.featureSpace ds) "y" ["x"]
            lr = LRGD.linearRegression' 0.005 ds ms
        it "should correctly estimate coefficients for some toy problem" $ do
            checkVector (LRGD.lrCoefficients lr) [-1, 2]

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
        let cat      = DS.createFeature "x1" [ "blue", "blue", "blue", "red", "red" ]
            oth      = DS.createFeature "x2" [ "13", "14", "15", "8", "7" ]
            yc       = DS.createFeature "y" [ "1.0", "1.1", "1.2", "0.3", "0.6" ]
            ds       = DS.createFromFeatures "hspec" [cat, oth, yc]
            Right ms = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
            lrFit    = linearRegression'' (<= 2) ds ms
        checkVector (lrCoefficients lrFit) [-0.3, 0.1] -- intercept only: color can't be used
        checkSingleCol (M.predict' lrFit ds (>=3)) [0.5, 0.4] -- intercept-based estimate


shouldRoughlyEqual :: (Show a, Num a, Ord a, Fractional a) => a -> a -> IO ()
shouldRoughlyEqual actual expected = actual `shouldSatisfy` roughlyEqual expected

roughlyEqual :: (Num a, Ord a, Fractional a) => a -> a -> Bool
roughlyEqual expected actual = 0.001 > abs (expected - actual)

checkSingleCol :: DS.Feature Double -> [Double] -> IO ()
checkSingleCol (DS.SingleCol DS.Column {..}) = checkVector $ V.convert colData
checkSingleCol (DS.MultiCol _)               = const $ fail "expecting quantitative column"

checkVector :: Vector Double -> [Double] -> IO ()
checkVector xs y = do
    V.length xs `shouldBe` length y
    zipWithM_ (\x ex -> x `shouldRoughlyEqual` ex) (V.toList xs) y

