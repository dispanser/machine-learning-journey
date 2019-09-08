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

rowSelectorFromList :: [Bool] -> RowSelector
rowSelectorFromList xs =
    let v = V.fromList xs
    in (v V.!)

negateRowSelector :: RowSelector -> RowSelector
negateRowSelector rs = \i -> not (rs i)

-- fit a model based on the provided input, splitting the data into training and
-- validation set based on the provided seed. Note: split is 50 / 50.
validateModel :: M.Predictor a =>
    (Dataset -> M.ModelSpec -> RowSelector -> a) ->
        Int -> Dataset -> M.ModelSpec -> Double
validateModel modelFit seed ds ms =
    let n         = DS.dsNumRows' ds
        trainRows = rowSelectorFromList $ validationSetSplit seed n
    in runWithTestSet modelFit ds ms trainRows

kFoldModel :: M.Predictor a =>
    (Dataset -> M.ModelSpec -> RowSelector -> a) ->
        Int -> Int -> Dataset -> M.ModelSpec -> Double
kFoldModel fit seed k ds ms =
    let n          = DS.dsNumRows' ds
        folds      = kFoldSplit seed n k
        fitFold k' = let trainRowS = rowSelectorFromList ((/= k') <$> folds)
                     in runWithTestSet fit ds ms trainRowS
    in sum (fitFold <$> [0..k-1]) / fromIntegral k

-- row selector selects the rows used for training the model. The negation
-- yields the rows that are used for computing the prediction error
runWithTestSet :: M.Predictor a =>
    (Dataset -> M.ModelSpec -> RowSelector -> a) ->
        Dataset -> M.ModelSpec -> RowSelector -> Double
runWithTestSet fit ds ms rs =
    let mFit         = fit ds ms rs
        testRS       = negateRowSelector rs
        prediction   = RU.head $ DS.featureVectors $ M.predict' mFit ds testRS
        testResponse = DS.colData . DS.filterDataColumn testRS $
            M.extractResponseVector ds ms
        testError    = V.sum $ V.map (^(2::Int)) $ V.zipWith (-)
            prediction testResponse
    in testError / fromIntegral (V.length prediction)

