{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ISL.LinearRegressionSuite where

import           Base
import qualified ML.Model as M
import           ML.Data.Summary
import           ML.Dataset.CSV (readRawData, readRawData')
import           ML.Dataset (Dataset(..))
import qualified ML.Dataset as DS
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified ML.LinearRegression as LR
import qualified ML.LinearRegressionGD as LRGD
import           ML.Data.Feature.Internal (ScaleStrategy)
import           ML.Data.Vector (scale01, normalize, noScaling)
import qualified Data.Vector.Storable as V

spec_babyRegression :: Spec
spec_babyRegression = do
    describe "linear-solver based  linear regression" $ do
        babyRegression (LR.fitLinearRegression)
    describe "gradient-descent based linear regression" $ do
        babyRegression $ LRGD.linearRegressionGD 0.1 2500

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

spec_ISLRLinearRegressionGD :: Spec
spec_ISLRLinearRegressionGD = parallel $
    describe "LR Gradient descent: Adertising dataset, ISLR chapter 3:" $ do
        -- this takes 4s and is by far (100x) slower than all other tests combined
        -- describe "simple linear regression for 'sales ~ TV'" $ do
        --     advertisingDataset <- runIO readAdvertisingDataset noScaling
        --     let Right ms = M.buildModelSpec
        --             (featureSpace advertisingDataset) "sales" ["TV"]
        --         model = LRGD.linearRegressionGD 0.0000003 600000 ms
        --         lr@LRGD.LinearRegressionGD {..} = M.fitDataset model advertisingDataset
        --     -- runIO $ print lr
        --     it "computes coefficients" $
        --         checkVector (LR.coefficients lr) [7.0325, 0.0475]

        --     it "computes rse" $
        --         LR.rse lr `shouldRoughlyEqual` 3.258

        --     it "computes R^2" $
        --         LR.r2 lr `shouldRoughlyEqual` 0.612
        scaledAdv <- runIO $ DS.parseScaledDataset scale01 <$>
                readRawData "data/Advertising.csv"
        describe "applying feature scaling" $ do
            let Right dsScaled = scaledAdv
            let Right ms = M.buildModelSpec
                    (featureSpace dsScaled) "sales" ["TV"]
                model = LRGD.linearRegressionGD 0.05 1400 ms
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
                model = LRGD.linearRegressionGD 0.05 2000 ms
                lr@LRGD.LinearRegressionGD {..} = M.fitDataset model dsScaled
            it "computes coefficents" $ do
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
        advertisingDataset <- runIO $ readAdvertisingDataset noScaling
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
            it "computes coefficents" $ do
                checkVector (LR.coefficients lr) [ 2.939, 0.046, -0.001, 0.189 ]

            it "computes rse" $
                LR.rse lr `shouldRoughlyEqual` 1.686

            it "computes standard errors" $
                checkVector lrStandardErrors [0.311908, 0.001395, 0.005871, 0.008611]

            it "computes F-Statistics" $
                LR.fStatistics lr `shouldRoughlyEqual` 570.271

            it "computes R^2" $
                LR.r2 lr `shouldRoughlyEqual` 0.897

spec_Regularization :: Spec
spec_Regularization = do
    ds <- runIO $ readHittersDataset normalize
    let Right ms = M.buildModelSpec (featureSpace ds) "Salary"
            [ "AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Years", "CAtBat"
            , "CHits", "CHmRun", "CRuns", "CRBI", "CWalks", "League", "Division"
            , "PutOuts", "Assists", "Errors", "NewLeague" ]
    describe "ridge regression" $ do
        -- it "produce the coefficents stated in the book for l=11498" $ do
        --     let model = LRGD.ridgeRegression 11498 0.00005 1000 ms
        --         lr    = M.fitDataset model ds
        --         coefs = LR.recoverOriginalCoefficients lr
        --     mapM_ print $ summary lr
        --     coefs `shouldBe` []
        it "produce the same coefficents as the unregularized LR for l=0" $ do
            let model = LRGD.ridgeRegression 0 0.001 10000 ms
                lr    = M.fitDataset model ds
                coefs = LR.recoverOriginalCoefficients lr
            checkList coefs [ 163.10, 0.37, -1.98, -0.17, 0.13, -0.17, 0.81
                            , 1.45, -0.81, -116.85, -3.36, 7.50, 4.33, 62.60
                            , -24.76, 0.28, -1.04, -2.38, 6.23, -3.49]
        it "produce the same coefficents as R for l=1" $ do
            let model = LRGD.ridgeRegression 1 0.001 20000 ms
                lr    = M.fitDataset model ds
                coefs = LR.recoverOriginalCoefficients lr
            mapM_ print $ summary lr
            checkList coefs [ 163.10, 0.37, -1.98, -0.17, 0.13, -0.17, 0.81
                            , 1.45, -0.81, -116.85, -3.36, 7.50, 4.33, 62.60
                            , -24.76, 0.28, -1.04, -2.38, 6.23, -3.49]
    describe "the lasso" $ do
        -- it "produce the coefficents stated in the book for l=11498" $ do
        --     let model = LRGD.ridgeRegression 11498 0.00005 1000 ms
        --         lr    = M.fitDataset model ds
        --         coefs = LR.recoverOriginalCoefficients lr
        --     mapM_ print $ summary lr
        --     coefs `shouldBe` []
        it "produce the same coefficents as the unregularized LR for l=0" $ do
            let model = LRGD.lassoRegression 0 0.001 10000 ms
                lr    = M.fitDataset model ds
                coefs = LR.recoverOriginalCoefficients lr
            checkList coefs [ 163.10, 0.37, -1.98, -0.17, 0.13, -0.17, 0.81
                            , 1.45, -0.81, -116.85, -3.36, 7.50, 4.33, 62.60
                            , -24.76, 0.28, -1.04, -2.38, 6.23, -3.49]
        it "produce the same coefficents as R for l=1" $ do
            let model = LRGD.lassoRegression 10 0.001 100000 ms
                lr    = M.fitDataset model ds
                coefs = LR.recoverOriginalCoefficients lr
            mapM_ print $ summary lr
            checkList coefs [ 163.10, 0.37, -1.98, -0.17, 0.13, -0.17, 0.81
                            , 1.45, -0.81, -116.85, -3.36, 7.50, 4.33, 62.60
                            , -24.76, 0.28, -1.04, -2.38, 6.23, -3.49]
    describe "gradient descent regression" $ do
        let originalCoefficients = [ 163.10, 0.37, -1.98, -0.17, 0.13, -0.17, 0.81
                                   , 1.45, -0.81, -116.85, -3.36, 7.50, 4.33, 62.60
                                   , -24.76, 0.28, -1.04, -2.38, 6.23, -3.49]
        it "produce the coefficents on non-constrained lr" $ do
            let model = LRGD.linearRegressionGD 0.001 9000 ms
                lr    = M.fitDataset model ds
                coefs = LR.recoverOriginalCoefficients lr
            checkList coefs originalCoefficients
        it "produce the coefficents stated in the book for OLS" $ do
            let model = LR.fitLinearRegression ms
                lr    = M.fitDataset model ds
                coefs = LR.recoverOriginalCoefficients lr
            checkList coefs originalCoefficients

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

readAdvertisingDataset :: ScaleStrategy -> IO Dataset
readAdvertisingDataset ss = do
    Right ds <- DS.parseScaledDataset ss <$> readRawData "data/Advertising.csv"
    return ds

readHittersDataset :: ScaleStrategy ->  IO Dataset
readHittersDataset ss = do
    Right ds <- DS.parseScaledDataset ss <$> readRawData' (/= "") "data/hitters/HittersNoSalaryNA.csv"
    return ds
