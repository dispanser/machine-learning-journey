{-# LANGUAGE OverloadedStrings #-}

module MathML.Calculus.M3.ILoveBackPropagation where


{-
    Mathematics for Machine Learning, Course on Multivariate Calculus, Coursera

    Building a basic neural network and train it using derivatives of the cost function, by hand.

    Note that we're following the provided notebook very closely, so there won't be any
    kind of interesting engineering. Baseline is python, you get what you pay for.
-}

import qualified Relude.Unsafe as RU
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R, (><))
import qualified Data.Vector.Storable as VS
import qualified ML.Dataset as DS
import qualified ML.Dataset.CSV as DSV
import qualified ML.NN01 as NN
import qualified Data.List.Split as S
import qualified Visuals.Image as VI
import qualified Codec.Picture as JP

-- the example network that is supposed to to negation, randomly initialized :)
notNNInit :: Double -> Double -> NN.NeuralNetwork
notNNInit w b =
    NN.NeuralNetwork [NN.Layer (M.scalar w) (M.scalar b) $
        NN.LayerSpec 1 NN.Tanh 0.1 ]

runNotNN :: NN.NeuralNetwork -> IO ()
runNotNN nn = do
    let xs  = (2><1) [0, 1]
        ys  = (2><1) [1, 0]
        ns  = NN.forwardNetwork nn xs ys
        bp  = NN.backprop ys ns
        -- nn  = updateNetwork 0.1 nn bp
    putStrLn $ "a[0, 1]  = " ++ show (NN.layerStates ns)
    putStrLn $ "backprop = " ++ show bp
    putStrLn $ "updated  = " ++ show (NN.updateNetwork nn bp)
    putStrLn $ "10 runs  = " ++ show (NN.train xs ys 5000 nn)

notNetMain :: IO ()
notNetMain =
    runNotNN $ notNNInit 1.3 (-0.1)

cost :: Matrix R -> Matrix R -> R
cost x y =  M.norm_2 . M.cmap (**2) $ x - y

main :: IO ()
main =
    iLoveBackPropagation

iLoveBackPropagation :: IO ()
iLoveBackPropagation = do
    nn <- NN.initializeNetwork $ NN.NetworkSpec 1 [
        NN.LayerSpec 6 NN.Tanh 0.005, NN.LayerSpec 7 NN.Tanh 0.003 , NN.LayerSpec 2 NN.Tanh 0.002 ]
    (inp, out) <- slice 100
    let nss = NN.train' inp out nn
    let n = 300
    mapM_ (print . NN.theta . snd) $ everyNth (n `div` 10) $ take n $  RU.tail nss
    let res = RU.last . take n $ nss
    let evalHeart = evalNetwork $ fst res
    print $ fst res
    -- JP.writePng  "/tmp/heart.png" $ VI.imageFromPixelMap $
    --     VI.generateTimeSeriesImage evalHeart 1024 768

evalNetwork :: NN.NeuralNetwork -> Double -> (Double, Double)
evalNetwork nn x =
    let nss = NN.forwardNetwork nn (M.scalar x) (M.fromColumns [M.fromList [0, 0]])
        res = NN.a . RU.last $ NN.layerStates nss
        [[x', y']] = VS.toList <$> M.toRows res
    in (x', 1-y')

slice :: Int -> IO (Matrix R, Matrix R)
slice n = do
    dataset <- readDataset
    let Just inp = M.fromColumns . (:[]) . VS.take n . VS.convert <$> DS.colByName' dataset "s"
    let Just out = M.fromColumns . (VS.take n . VS.convert <$>) <$> sequence [
            DS.colByName' dataset "x", DS.colByName' dataset "y"]
    return (inp, out)

readDataset :: IO DS.Dataset
readDataset = do
    Right ds <- DS.parseFullDataset <$> DSV.readRawData "data/mml/ILoveBackPropagation.csv"
    return ds

everyNth :: Int -> [a] -> [a]
everyNth n xs = RU.head <$> S.chunksOf n xs
