module ISL.Model.Validation
    ( kFoldSplit
    , validationSetSplit
    , validateModel)

     where

import qualified ISL.Model as M
import qualified System.Random as Rand
import qualified Data.Vector as V

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
validateModel :: (M.Predictor a, M.ModelFit a) => Int -> M.ModelInput -> (a, Double)
validateModel seed mi =
    let trainRows             = validationSetSplit seed (V.length . M.colData . M.miResponse $ mi)
        (trainData, testData) = M.splitModelInput trainRows mi
        modelFit              = M.fit trainData
        prediction            = M.colData $ M.predict modelFit $ M.miFeatures testData
        testError             = V.sum $ V.map (^(2::Int)) $ V.zipWith (-)
            prediction (M.colData . M.miResponse $ testData)
    in (modelFit, testError / fromIntegral (V.length prediction))
