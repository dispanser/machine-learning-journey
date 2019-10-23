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
import           Numeric.LinearAlgebra (Matrix, R, (><))
import qualified ML.Data.Generate as DG
import qualified ML.Dataset as DS
import qualified ML.Dataset.CSV as DSV
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import           System.Random.MWC (Gen, create)

type LearnRate = Double

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

data LayerGradients = LayerGradients
    { jw :: Matrix R
    , jb :: Vector R } deriving Show

data NetworkState = NetworkState
    { a0          :: Matrix R
    , layerStates :: [LayerState] } deriving Show

newtype NN     = NN { unLayers :: [Layer] } deriving Show
newtype NNSpec = NNSpec { unSpec :: [(Int, Int)] }

type Sigmoid a = a -> a

-- fw' w b = M.sumElements $ 0.5 * 2 * (fw w b - y) * (1/cosh(z' w b)**2) * a0'
-- fb' w b = M.sumElements $ 0.5 * 2 * (fw w b - y) * (1/cosh(z' w b)**2)

backprop :: Sigmoid R -> Matrix R -> NetworkState -> [LayerGradients]
backprop sg' ys NetworkState { .. } = backprop' a0 layerStates
 where
    backprop' :: Matrix R -> [LayerState] -> [LayerGradients]
    backprop' xs [LayerState{..}] =
        let n = fromIntegral $ M.rows ys
            dCda = M.scale (2/n) (a - ys) -- n x p - n x p -> n x p
            dadz = M.cmap sg' z           -- n x p -> n x p
            -- dzdw is the jacobian containing the partial derivative for each element w_ij of w.
            -- each column is identical, because they all only depend on the previous layer outputs.
            -- each element in the i'ths row is the average of the input activation levels of the
            -- associated a_i neuron from the previousl layer.
            dCdz = dCda * dadz
            jw   = M.scale n $ M.tr dCdz <> xs
            jb   = VS.fromList $ (/n) . VS.sum <$> M.toColumns dCdz

        in [LayerGradients { .. }]
    backprop' _  []               = []
    backprop' xs (l:ls)           =
        let bps@(lastBS:_) = backprop' (a l) ls
        in undefined

-- process one layer by taking its input values (matrix n x m)
-- and produce the outputs of the current layer (matrix n x p)
-- on a layer with p neurons and m neurons in the previous layer
forwardLayer :: Sigmoid R -> Layer -> Matrix R -> LayerState
forwardLayer sg Layer {..} x =
    let z = (x <> w) + M.fromColumns [b]
        a = M.cmap sg z
    in LayerState { .. }

forwardNetwork :: Sigmoid R -> NN -> Matrix R -> NetworkState
forwardNetwork sg NN { .. } x = NetworkState x $ go x unLayers
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

-- update the weights in the layer according to the gradients stored
-- in the backprop structure.
updateLayer :: LearnRate -> Layer -> LayerGradients -> Layer
updateLayer lr l g = l
    { w = w l - M.scale lr (jw g)
    , b = b l - M.scale lr (jb g) }

updateNetwork :: LearnRate -> NN -> [LayerGradients] -> NN
updateNetwork lr nn gs = nn
    { unLayers = zipWith (updateLayer lr) (unLayers nn) gs }


-- the example network that is supposed to to negation, randomly initialized :)
notNNInit :: Double -> Double -> NN
notNNInit w b = NN [Layer 1 1 (M.scalar w) (M.scalar b)]

train :: LearnRate -> Matrix R -> Matrix R -> Int -> NN -> NN
train lr xs ys n = RU.head . drop (n-1) . iterate step
 where step nn = let ns  = forwardNetwork tanh nn xs
                     bp  = backprop tanh' ys ns
                 in  updateNetwork lr nn bp

runNotNN :: NN -> IO ()
runNotNN nn = do
    let xs  = (2><1) [0, 1]
        ys  = (2><1) [1, 0]
        ns = forwardNetwork tanh nn xs
        bp  = backprop tanh' ys ns
        -- nn  = updateNetwork 0.1 nn bp
    putStrLn $ "a[0, 1]  = " ++ show (layerStates ns)
    putStrLn $ "backprop = " ++ show bp
    putStrLn $ "updated  = " ++ show (updateNetwork 1 nn bp)
    putStrLn $ "10 runs  = " ++ show (train 0.1 xs ys 5000 nn)

tanh' :: Sigmoid R
tanh' x = (1/cosh x) ** 2

notNetMain :: IO ()
notNetMain = do
    runNotNN $ notNNInit 1.3 (-0.1)
    -- runNotNN $ notNNInit (-1) 1
    -- runNotNN $ notNNInit (-5) 5

cost :: Matrix R -> Matrix R -> R
cost x y =  M.norm_2 . M.cmap (**2) $ x - y

main :: IO ()
main = do
    nn <- initializeNetwork $ NNSpec [(1, 6), (6, 7), (7, 2)]
    notNetMain

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
