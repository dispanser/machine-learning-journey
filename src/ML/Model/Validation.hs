{-# LANGUAGE RecordWildCards #-}

module ML.Model.Validation
    ( kFoldSplit
    , validationSetSplit
    , validateModel
    , negateRowSelector
    , kFoldModel)

     where

import qualified Relude.Unsafe as RU
import qualified ML.Model as M
import           ML.Dataset (Dataset, RowSelector)
import qualified ML.Dataset as DS
import qualified System.Random as Rand
import qualified Data.Vector.Unboxed as V

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

negateRowSelector :: RowSelector -> RowSelector
negateRowSelector rs = \i -> not (rs i)

kFoldModel :: M.Model a => M.ModelInit a -> Int -> Int -> Dataset -> Double
kFoldModel mi@M.ModelInit { .. } seed k ds =
    let n          = DS.dsNumRows' ds
        folds      = kFoldSplit seed n k
        fitFold k' = let trainRowS = DS.rowSelectorFromList ((/= k') <$> folds)
                     in runWithTestSet mi ds trainRowS
    in sum (fitFold <$> [0..k-1]) / fromIntegral k

validateModel :: M.Model a => M.ModelInit a -> Int -> Dataset -> Double
validateModel modelFit seed ds =
    let n         = DS.dsNumRows' ds
        trainRows = DS.rowSelectorFromList $ validationSetSplit seed n
    in runWithTestSet modelFit ds trainRows

runWithTestSet :: M.Model a => M.ModelInit a -> Dataset -> RowSelector -> Double
runWithTestSet mi@M.ModelInit { .. } ds rs =
    let mFit         = M.fitSubset mi rs ds
        testRS       = negateRowSelector rs
        prediction   = RU.head $ DS.featureVectors $ M.predictSubset mFit testRS ds
        testResponse = DS.colData . DS.filterDataColumn testRS $
            M.extractResponseVector ds modelSpec
        testError    = V.sum $ V.map (^(2::Int)) $ V.zipWith (-)
            prediction testResponse
    in testError / fromIntegral (V.length prediction)
