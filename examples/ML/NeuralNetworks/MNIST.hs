{-# LANGUAGE RecordWildCards #-}
{-
   based on: http://neuralnetworksanddeeplearning.com/chap1.html
-}

module ML.NeuralNetworks.MNIST where

import qualified Relude.Unsafe as RU
import qualified Datasets.MNIST        as MN
import qualified ML.NN01               as NN
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R)
import           Data.Time.Clock (getCurrentTime, diffUTCTime)

data TestData = TestData
    { trainingInput  :: Matrix R
    , trainingLabels :: Matrix R
    , testInput      :: Matrix R
    , testLabels     :: Matrix R }

trainImagePath, testImagePath, trainLabelPath, testLabelPath :: FilePath
trainImagePath = "/fs/data/data/study/datasets/mnist/train-images-idx3-ubyte"
testImagePath  = "/fs/data/data/study/datasets/mnist/t10k-images-idx3-ubyte"
trainLabelPath = "/fs/data/data/study/datasets/mnist/train-labels-idx1-ubyte"
testLabelPath  = "/fs/data/data/study/datasets/mnist/t10k-labels-idx1-ubyte"

loadMNist :: IO TestData
loadMNist = do
    trainImages <- M.fromRows . (MN.convertImage <$>) <$> MN.readImages trainImagePath
    trainLabels <- M.fromRows . (NN.encodeOneHot <$>) <$> MN.readLabels trainLabelPath
    testImages  <- M.fromRows . (MN.convertImage <$>) <$> MN.readImages testImagePath
    testOutput  <- M.fromRows . (NN.encodeOneHot <$>) <$> MN.readLabels testLabelPath
    return TestData
        { trainingInput  = trainImages
        , trainingLabels = trainLabels
        , testInput      = testImages
        , testLabels     = testOutput }

trainMNist :: [Int] -> TestData -> Int -> Int -> IO NN.NeuralNetwork
trainMNist ds td epochs batchSize = do
    nn <- NN.initializeNetwork $ createNetworkArchitecture ds
    print $ "train @ batchSize = " ++ show batchSize ++ " network: " ++ NN.printNetworkConfig nn
    let images = M.toRows $ trainingInput td
        labels = M.toRows $ trainingLabels td
        epochResults = take epochs $ iterate (NN.epoch images labels batchSize) nn
    mapM_ (uncurry $ evaluateModel td) (zip [1..] epochResults)
    return $ RU.last epochResults

-- create a network for mnist using the provided list of ints as neurons in dense (hidden) layers.
createNetworkArchitecture :: [Int] -> NN.NetworkSpec
createNetworkArchitecture ds =
    NN.NetworkSpec 784 $ map (\d -> NN.LayerSpec d NN.Sigmoid 0.02)
        ds ++ [NN.LayerSpec 10 NN.Sigmoid 0.02]

evaluateModel :: TestData -> Int -> NN.NeuralNetwork -> IO ()
evaluateModel TestData { .. } epoch nn = do
    -- let trainNS = NN.forwardNetwork nn trainingInput trainingLabels
    -- print $ show epoch ++ "train error: " ++ show (classificationError trainingLabels trainNS * 100) ++ "%"
    let testNS = NN.forwardNetwork nn testInput testLabels
    print $ show epoch ++ " test  error: " ++ show (classificationError testLabels testNS * 100) ++ "%"

classificationError :: Matrix R -> NN.NetworkState -> Double
classificationError ys ns =
    let n            = M.rows ys
        res          = M.toRows $ NN.result ns
        yHat         = M.fromList $ NN.decodeOneHot <$> res
        y            = M.fromList $ NN.decodeOneHot <$> M.toRows ys
        correctPreds = NN.numCorrect yHat y
        wrongPreds   = n - correctPreds
    in fromIntegral wrongPreds / fromIntegral n

evaluateMNist :: [Int] -> Int -> Int -> IO ()
evaluateMNist ds epochs batchSize = do
    testData@TestData {..}  <- loadMNist
    _ <- timed ("finished @ batch size " ++ show batchSize) $ trainMNist ds testData epochs batchSize
    return ()

timed :: Show a => a -> IO b -> IO b
timed desc action = do
    startAt <- getCurrentTime
    actionR <- action
    endAt   <- getCurrentTime
    let timePassed = diffUTCTime endAt startAt
    print $ show desc ++ " in " ++ show timePassed
    return actionR

main :: IO ()
main = do
    evaluateMNist [32] 3 1
    -- evaluateMNist 90 2
    -- evaluateMNist 135 4
    -- evaluateMNist 230 8
    -- evaluateMNist 300 16
    -- evaluateMNist 375 32
    -- evaluateMNist 420 64
