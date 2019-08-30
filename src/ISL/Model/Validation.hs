module ISL.Model.Validation
    ( kFoldSplit
    , validationSetSplit
    , validateModel
    , kFoldModel)

     where

import qualified ISL.Model as M
import qualified System.Random as Rand
import qualified Data.Vector as V
import qualified Debug.Trace as D

-- create a list of booleans that can be used for a validation set split
validationSetSplit :: Int -> Int -> [Bool]
validationSetSplit seed n =
    let rg = Rand.mkStdGen seed
    in take n $ Rand.randoms rg

-- create a list of ints in range [0, k-1] that can be used as a split
-- for k-fold cross validation
kFoldSplit :: Int -> Int -> Int -> [Int]
kFoldSplit seed n k =
    let rg = Rand.mkStdGen seed
    in take n $ Rand.randomRs (0, k-1) rg

-- fit a model based on the provided input, splitting the data into training and
-- validation set based on the provided seed. Note: split is 50 / 50.
validateModel :: M.Predictor a => (M.ModelInput -> a) -> Int -> M.ModelInput -> Double
validateModel modelFit seed mi =
    let trainRows             = validationSetSplit seed (V.length . M.colData . M.miResponse $ mi)
        (trainData, testData) = M.splitModelInput trainRows mi
    in runWithTestSet modelFit trainData testData

kFoldModel :: (M.Predictor a, M.ModelFit a) => (M.ModelInput -> a) -> Int -> Int -> M.ModelInput -> Double
kFoldModel fit seed k mi =
    let folds      = kFoldSplit seed (V.length . M.colData . M.miResponse $ mi) k
        fitFold k' = let trainRows             = (== k') <$> folds
                         (testData, trainData) = M.splitModelInput trainRows mi
                     in runWithTestSet fit trainData testData
    in sum (fitFold <$> [0..k-1]) / fromIntegral k

runWithTestSet :: M.Predictor a => (M.ModelInput -> a) -> M.ModelInput -> M.ModelInput -> Double
runWithTestSet fit trainData testData =
    let modelFit              = fit trainData
        prediction            = M.colData $ M.predict modelFit $ M.miFeatures testData
        testError             = D.traceShow (V.length prediction) $ V.sum $ V.map (^(2::Int)) $ V.zipWith (-)
            prediction (M.colData . M.miResponse $ testData)
    in testError / fromIntegral (V.length prediction)

