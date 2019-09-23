{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ISL.LinearRegressionSuite where

import           Base
import qualified ML.Model as M
import           ML.Dataset.CSV (readRawData)
import           ML.Dataset (Dataset(..))
import qualified ML.Dataset as DS
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified ML.LinearRegression as LR
import qualified ML.LinearRegressionGD as LRGD
import           ML.Data.Feature.Internal (scaled01)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as VU

spec_babyRegression :: Spec
spec_babyRegression = do
    describe "linear-solver based  linear regression" $ do
        babyRegression (LR.fitLinearRegression)
    describe "gradient-descent based linear regression" $ do
        babyRegression $ LRGD.linearRegressionGD 0.018 2500

babyRegression :: M.Predictor a => (M.ModelSpec -> M.ModelInit a) -> Spec
babyRegression cfgF = do
    describe "on some toy example" $ do
        -- perfect line: y = 2x-1
        let x         = contFeat "x" [2.0, 3.0, 4.0 :: Double]
            y         = contFeat "y" [3.0, 5.0, 7.0 :: Double]
            xTest     = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
            ds        = DS.createFromFeatures "hspec" [x, y]
            Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x"]
            predictor = M.fitDataset (cfgF ms) ds
        it "should correctly predict an unseen point" $ do -- black-box testing
            let (DS.Feature _ [res]) = M.predict predictor [xTest]
            checkVector res [-1, 1 .. 9]

spec_TestingRescalingBehavior :: Spec
spec_TestingRescalingBehavior = do
    describe "baseline" $ do
        let x1Data = VU.fromList [5.0, 4.0, 4.5, 7, 13]
            x2Data = VU.fromList [8, 18, 9, 4, 3]
            yData  = VU.fromList [4, 6, 4, 3, 2]
            b0 = 2.962
            b1 = -0.1164
            b2 = 0.192606
        it "should compute some coefficients" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" x2Data
                y         = contFeat "y"  yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) [b0, b1, b2]
        it "should halve b2 when scaling column 2 up by 2" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" $ VU.map (*2) x2Data
                y         = contFeat "y"  yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) [b0, b1, b2 / 2]
        it "should substract b2 from b0 when shifting column 2 up by 1" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" $ VU.map (+1) x2Data
                y         = contFeat "y"  yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) [b0 - b2, b1, b2]
        it "should adapt b0 and b2 without touching b1 on linear transformation of x2" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" $ VU.map ((/3) . (subtract 5)) x2Data
                y         = contFeat "y"  yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) [b0+5*b2, b1, 3*b2]
        it "should adapt all of beta when multiplying y with a scalar" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" x2Data
                y         = contFeat "y"  $ VU.map (/3) yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) $ (/3) <$> [b0, b1, b2]
        it "should adapt intercept when shifting y" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" x2Data
                y         = contFeat "y"  $ VU.map (+3) yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) [b0 + 3, b1, b2]
        it "should do something to all of you  y" $ do
            let x1        = contFeat "x1" x1Data
                x2        = contFeat "x2" x2Data
                y         = contFeat "y"  $ VU.map ((/7) . subtract 3) yData
                ds        = DS.createFromFeatures "hspec" [x1, x2, y]
                Right ms  = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                lrModel   = M.fitDataset (LR.fitLinearRegression ms) ds
            checkVector (LR.coefficients lrModel) $ (/7) <$> [b0 - 3, b1, b2]

spec_ISLRLinearRegressionGD :: Spec
spec_ISLRLinearRegressionGD = parallel $
    describe "LR Gradient descent: Adertising dataset, ISLR chapter 3:" $ do
        -- this takes 4s and is by far (100x) slower than all other tests combined
        -- describe "simple linear regression for 'sales ~ TV'" $ do
        --     advertisingDataset <- runIO readAdvertisingDataset
        --     let Right ms = M.buildModelSpec
        --             (featureSpace advertisingDataset) "sales" ["TV"]
        --         model = LRGD.linearRegressionGD $ LRGD.ModelConfig 0.0000003 (LRGD.maxIterations 600000) ms
        --         lr@LRGD.LinearRegressionGD {..} = M.fitDataset model advertisingDataset
        --     runIO $ print lr
        --     it "computes coefficients" $
        --         checkVector (LR.coefficients lr) [7.0325, 0.0475]

        --     it "computes rse" $
        --         LR.rse lr `shouldRoughlyEqual` 3.258

        --     it "computes R^2" $
        --         LR.r2 lr `shouldRoughlyEqual` 0.612
        scaledAdv <- runIO $ DS.parseDataset
            [ scaled01 "TV", scaled01 "radio"
            , scaled01 "newspaper", scaled01 "sales" ] <$>
                readRawData "data/Advertising.csv"
        describe "applying feature scaling" $ do
            let Right dsScaled = scaledAdv
            let Right ms = M.buildModelSpec
                    (featureSpace dsScaled) "sales" ["TV"]
                model = LRGD.linearRegressionGD 0.005 140 ms
                lr@LRGD.LinearRegressionGD {..} = M.fitDataset model dsScaled
            let [intercept, tvCoef] = V.toList $ LR.coefficients lr

            -- recover original coefficents from unscaled solution.
            let coef'      = tvCoef * 25.4 / 295.7
            let intercept' = intercept * 25.4 + 1.6 - 0.7 * coef'

            it "recovers original coefficients" $ do
                checkList (LR.recoverOriginalCoefficients lr) [7.0325, 0.0475]

            it "scales back tv coefficient" $
                coef' `shouldRoughlyEqual` 0.0475

            it "scales back intercept" $
                intercept' `shouldRoughlyEqual` 7.0325

            it "computes rse" $
                LR.rse lr `shouldRoughlyEqual` 3.258

            it "computes R^2" $
                LR.r2 lr `shouldRoughlyEqual` 0.612
        describe "multivariate OLS for 'sales ~ TV + Radio + Newsaper'" $ do
            let Right dsScaled = scaledAdv
                Right ms = M.buildModelSpec
                    (featureSpace dsScaled) "sales" [
                        "TV", "radio", "newspaper"]
                model = LRGD.linearRegressionGD 0.005 500 ms
                lr@LRGD.LinearRegressionGD {..} = M.fitDataset model dsScaled
            it "computes coeffiicents" $ do
                checkList (LR.recoverOriginalCoefficients lr) [ 2.939, 0.046, -0.001, 0.189 ]

            it "computes rse" $
                LR.rse lr `shouldRoughlyEqual` 1.686

            it "computes F-Statistics" $
                LR.fStatistics lr `shouldRoughlyEqual` 570.271

            it "computes R^2" $
                LR.r2 lr `shouldRoughlyEqual` 0.897

spec_ISLRLinearRegression :: Spec
spec_ISLRLinearRegression = parallel $
    describe "Adertising dataset, ISLR chapter 3:" $ do
        advertisingDataset <- runIO readAdvertisingDataset
        describe "simple linear regression for 'sales ~ TV'" $ do
            let Right ms = M.buildModelSpec
                    (featureSpace advertisingDataset) "sales" ["TV"]
                model = LR.fitLinearRegression ms
                lr@LR.LinearRegression {..} = M.fitDataset model advertisingDataset
            it "computes coefficients" $
                checkVector (LR.coefficients lr) [7.0325, 0.0475]

            it "computes rse" $
                LR.rse lr `shouldRoughlyEqual` 3.258

            it "computes R^2" $
                LR.r2 lr `shouldRoughlyEqual` 0.612

            it "computes standard errors" $
                checkVector lrStandardErrors [0.4578, 0.0027]

            it "computes t-statistics" $
                checkVector (LR.tStatistics lr) [15.36, 17.667]

            let pValueF = LR.pValue $ fromIntegral lrDF

            it "computes p-values" $
                checkVector (V.map pValueF $ LR.tStatistics lr) [0.0, 0.0]

        describe "multivariate OLS for 'sales ~ TV + Radio + Newsaper'" $ do
            let Right ms = M.buildModelSpec
                    (featureSpace advertisingDataset) "sales" [
                        "TV", "radio", "newspaper"]
                model = LR.fitLinearRegression ms
                lr@LR.LinearRegression {..} = M.fitDataset model advertisingDataset
            it "computes coeffiicents" $ do
                checkVector (LR.coefficients lr) [ 2.939, 0.046, -0.001, 0.189 ]

            it "computes rse" $
                LR.rse lr `shouldRoughlyEqual` 1.686

            it "computes standard errors" $
                checkVector lrStandardErrors [0.311908, 0.001395, 0.005871, 0.008611]

            it "computes F-Statistics" $
                LR.fStatistics lr `shouldRoughlyEqual` 570.271

            it "computes R^2" $
                LR.r2 lr `shouldRoughlyEqual` 0.897

spec_EmptyClassLinearRegression :: Spec
spec_EmptyClassLinearRegression = do
    -- when only training on a subset of the data, it can thusly happen that
    -- some one-hot encoded class vector is 0, leading to a singular matrix
    -- and no solution to the linear equation
    describe "linear-solver based  linear regression" $ do
        it "should seemlessly handle empty class variables" $ do
            let cat      = catFeat "x1"  [ "blue", "blue", "blue", "red", "red" ]
                oth      = contFeat "x2" [ 13, 14, 15, 8, 7 ]
                yc       = contFeat "y"  [ 1.0, 1.1, 1.2, 0.3, 0.6 ]
                ds       = DS.createFromFeatures "hspec" [cat, oth, yc]
                Right ms = M.buildModelSpec (DS.featureSpace ds) "y" ["x1", "x2"]
                model    = LR.fitLinearRegression ms
                lrFit    = M.fitSubset model (<= 2) ds
                (DS.Feature _ [prediction]) = M.predictSubset lrFit (>=3) ds
            checkVector (LR.coefficients lrFit) [-0.3, 0.1] -- intercept and oth only
            checkVector prediction [0.5, 0.4]

readAdvertisingDataset :: IO Dataset
readAdvertisingDataset = do
    Right ds <- DS.parseFullDataset <$> readRawData "data/Advertising.csv"
    return ds

