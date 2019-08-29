module ISL.Model.Validation where

import qualified ISL.Model as M
import qualified System.Random as Rand

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

runValidation :: (M.Predictor a, M.ModelFit a) => Int -> M.ModelInput -> a
runValidation _seed _mi = undefined
