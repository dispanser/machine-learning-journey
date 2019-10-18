{-# LANGUAGE RecordWildCards #-}

module MathML.Calculus.M3.ILoveBackPropagation where


{-
    Mathematics for Machine Learning, Course on Multivariate Calculus, Coursera

    Building a basic neural network and train it using derivatives of the cost function, by hand.

    Note that we're following the provided notebook very closely, so there won't be any
    kind of interesting engineering. Baseline is python, you get what you pay for.
-}

import qualified Relude.Unsafe as RU
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R, (<#), (><))
import qualified ML.Data.Generate as DG
import qualified ML.Dataset as DS
import qualified ML.Dataset.CSV as DSV
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import           System.Random.MWC (Gen, create)

-- simplest possible representation of a layer fully connected to previous
data Layer = Layer -- fully connected, for now
    { n :: Int -- number of neurons in this layer
    , m :: Int -- number of neurons in the previous layer
    , w :: Matrix R -- m x p matrix of weights
    , b :: Vector R -- 1 x p
    } deriving Show

-- reference implementation uses a 7-tuple to represent the states of the four
-- layers of activations and the three intermediate linear functions,
-- [a0, z1, a1, z2, a2, z3, a3] where zi = wi*a(i-1) + bi, ai = sig(zi)
data LayerState = LayerState
    { a :: Matrix R
    , z :: Matrix R } deriving Show

data BackPropState = BackPropState
    { jw :: Matrix R
    , jb :: Matrix R } deriving Show

data NetworkState = NetworkState
    { a0          :: Vector R
    , layerStates :: [LayerState] }

newtype NN     = NN { unLayers :: [Layer] }
newtype NNSpec = NNSpec { unSpec :: [(Int, Int)] }

type Sigmoid a = a -> a

-- process one layer by taking its input values (vector of length m)
-- and produce the outputs of the current layer (vector of length n)
forwardLayer :: Sigmoid R -> Layer -> Matrix R -> LayerState
forwardLayer sg Layer {..} x =
    let z = (x <> w) + M.fromColumns [b]
        a = M.cmap sg z
    in LayerState { .. }

-- process multiple input vectors (n x m) on a neural network with p
-- outputs, resulting  in a n x p matrix of results.
forwardAll :: Sigmoid R -> NN -> Matrix R -> Matrix R
forwardAll sg NN {..} xs =
    let fwd xs' l = M.cmap sg $ (xs' <> w l) + M.fromColumns [b l]
    in foldl' fwd xs unLayers

forwardNetwork :: Sigmoid R -> NN -> Matrix R -> [LayerState]
forwardNetwork sg NN { .. } x = go x unLayers
    where go _ []      = []
          go x' (l:ls) =
              let st = forwardLayer sg l x'
              in st : go (a st) ls

initializeLayer :: (PrimMonad m) => Gen (PrimState m) -> Int -> Int -> m Layer
initializeLayer rg m n = do
    b  <- DG.standardNormalV rg n
    w  <- DG.standardNormalM rg m n
    return Layer { .. }

initializeNetwork :: NNSpec -> IO NN
initializeNetwork NNSpec {..} = do
    rg       <- create
    unLayers <- forM unSpec $ uncurry (initializeLayer rg)
    return NN { .. }

-- the example network that is supposed to to negation, randomly initialized :)
notNN :: NN
notNN = NN [Layer 1 1 (M.scalar 1.3) (M.scalar (-0.1))]

notNetMain :: IO ()
notNetMain = do
    let a0 = forwardLayer tanh (RU.head $ unLayers notNN) (M.fromLists [[0]])
        a1 = forwardLayer tanh (RU.head $ unLayers notNN) (M.fromLists [[1]])
    putStrLn $ "a(0) = " ++ show a0
    putStrLn $ "a(1) = " ++ show a1
    let a0' = forwardNetwork tanh notNN (M.fromLists [[0]])
    let a1' = forwardNetwork tanh notNN (M.fromLists [[1]])
    putStrLn $ "a(0) = " ++ show a0'
    putStrLn $ "a(1) = " ++ show a1'
    let a01 = forwardNetwork tanh notNN ((1><2) [0, 1])
    putStrLn $ "a[0, 1] = " ++ show a01

cost :: Matrix R -> Matrix R -> R
cost x y =  M.norm_2 . M.cmap (**2) $ x - y

main :: IO ()
main = do
    nn <- initializeNetwork $ NNSpec [(1, 6), (6, 7), (7, 2)]
    notNetMain
    print "Hello World"

-- sigmoid: logistic function. operates on a scalar (in contrast to numpy def)
-- because we'll just map that over our vector.
sig :: Floating a => a -> a
sig z = 1/(1+exp z)

-- because we can: point free, but arguably worse readability (YMMV)
sig' :: Floating a => a -> a
sig' = (1/) . (1+) . exp

-- derivative of our particular sigmoid
dSigma :: Floating a => a -> a
dSigma z = cosh(z/2)^(-2::Int) / 4

readDataset :: IO DS.Dataset
readDataset = do
    Right ds <- DS.parseFullDataset <$> DSV.readRawData "data/mml/ILoveBackPropagation.csv"
    return ds
