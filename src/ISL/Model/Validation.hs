module ISL.Model.Validation
    ( kFoldSplit
    , validationSetSplit
    -- , validateModel
    , kFoldModel)

     where

import qualified Relude.Unsafe as RU
import qualified ISL.Model as M
import           ML.DataSet (DataSet', RowSelector)
import qualified ML.DataSet as DS
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

rowSelectorFromList :: [Bool] -> RowSelector a
rowSelectorFromList xs =
    let v = V.fromList xs
    in (v V.!)

negateRowSelector :: RowSelector a -> RowSelector a
negateRowSelector rs = \i -> not (rs i)
-- fit a model based on the provided input, splitting the data into training and
-- validation set based on the provided seed. Note: split is 50 / 50.
-- validateModel :: M.Predictor a => (M.ModelInput -> a) -> Int -> M.ModelInput -> Double
-- validateModel modelFit seed mi =
--     let trainRows             = validationSetSplit seed $ M.miRows mi
--         (trainData, testData) = M.splitModelInput trainRows mi
--     in runWithTestSet modelFit trainData testData

kFoldModel :: M.Predictor a =>
    (DataSet' -> M.ModelSpec -> RowSelector Double -> a) ->
        Int -> Int -> DataSet' -> M.ModelSpec -> Double
kFoldModel fit seed k ds ms =
    let n          = DS.dsNumRows' ds
        folds      = kFoldSplit seed n k
        fitFold k' = let trainRowS = rowSelectorFromList ((/= k') <$> folds)
                     in runWithTestSet fit ds ms trainRowS
    in sum (fitFold <$> [0..k-1]) / fromIntegral k

-- row selector is the one that actively selects the rows used for
-- training the model. The negation yields the rows that are used
-- for computing the prediction error
runWithTestSet :: M.Predictor a =>
    (DataSet' -> M.ModelSpec -> RowSelector Double -> a) ->
        DataSet' -> M.ModelSpec -> RowSelector Double -> Double
runWithTestSet fit ds ms rs =
    let mFit         = fit ds ms rs
        testRS       = negateRowSelector rs
        prediction   = RU.head $ DS.featureVectors $ M.predict' mFit ds testRS
        testResponse = DS.colData . DS.filterDataColumn testRS $
            M.extractResponseVector ds ms
        testError    = V.sum $ V.map (^(2::Int)) $ V.zipWith (-)
            prediction testResponse
    in testError / fromIntegral (V.length prediction)

