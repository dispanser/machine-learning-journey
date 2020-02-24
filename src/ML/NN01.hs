{-# LANGUAGE RecordWildCards #-}
{-
   very basic implementation of neural networks, based on 10 Days of Grad, Day 1
-}

module ML.NN01 where

import qualified Relude.Unsafe as RU
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.List.Split as S
import qualified Data.Vector.Storable as VS
import qualified ML.Data.Generate as DG
import           Numeric.LinearAlgebra (Matrix, R)
import qualified Numeric.LinearAlgebra as M
import           Numeric.Morpheus.MatrixReduce (columnSum)
import qualified Numeric.Morpheus.Activation as MA
import           System.Random.MWC (Gen, create)

type LearnRate = Double

-- simplest possible representation of a layer fully connected to previous layer
data Layer = Layer -- fully connected, for now
    { w    :: Matrix R -- m x n matrix of weights
    , b    :: Matrix R -- 1 x n bias vector
    , spec :: LayerSpec
    } deriving Show

-- a_i = sig(z_i)
-- z_i = w_i * a_i-1
data LayerState = LayerState
    { a :: Matrix R
    , z :: Matrix R
    , layer :: Layer } deriving Show

-- gradients w respect to w, b of a specific layer
data LayerGradients = LayerGradients
    { jw :: Matrix R
    , jb :: Matrix R
    , dCdz :: Matrix R } deriving Show

data NetworkState = NetworkState
    { a0          :: Matrix R
    , theta       :: !Double
    , layerStates :: ![LayerState] } deriving Show

newtype NeuralNetwork = NeuralNetwork { unLayers :: [Layer] } deriving Show

data LayerSpec = LayerSpec
    { size  :: Int
    , sigm  :: Activation
    , alpha :: R } deriving Show

-- network spec: number of inputs, number of outputs, and n numbers
-- representing n hidden layers w/ n potentially differnt sigmoids
data NetworkSpec   = NetworkSpec
    { inputDims  :: Int
    , layerSpecs :: [LayerSpec] } deriving Show

data Activation = ReLu | Tanh | Sigmoid deriving (Show, Eq)

sigmoid :: Activation -> Matrix R -> Matrix R
sigmoid Tanh = MA.tanh_
sigmoid ReLu = MA.relu
sigmoid Sigmoid = MA.sigmoid

sigmoid' :: Activation -> Matrix R -> Matrix R
sigmoid' Tanh = MA.tanhGradient
sigmoid' ReLu = MA.reluGradient
sigmoid' Sigmoid = MA.sigmoidGradient

backprop :: Matrix R -> NetworkState -> [LayerGradients]
backprop ys NetworkState { .. } = backprop' a0 layerStates
 where
    backprop' :: Matrix R -> [LayerState] -> [LayerGradients]
    backprop' _  []               = []
    backprop' xs [LayerState{..}] =
        let dCda  = M.scale (2 / fromIntegral (M.rows a)) (a - ys) -- n x p - n x p -> n x p
            dadz  = (sigmoid' . sigm . spec $ layer) z -- n x p -> n x p
            dCdz' =  dCda * dadz
        in [ LayerGradients { jw   = M.tr xs <> dCdz'
                            , jb   = M.asRow $ columnSum dCdz'
                            , dCdz = dCdz' } ]
    backprop' xs (l:l':ls) =
        let bps@(lastBS:_) = backprop' (a l) $ l':ls
            sg'            = sigmoid' . sigm . spec . layer $ l
            dadz           = sg' $ z l
            wLast          = w (layer l')
            dCdz'          = (dCdz lastBS <> M.tr wLast) * dadz
        in LayerGradients { jw   = M.tr xs <> dCdz'
                          , jb   = M.asRow $ columnSum dCdz'
                          , dCdz = dCdz'
                          } : bps

-- process one layer by taking its input values (matrix n x m)
-- and produce the outputs of the current layer (matrix n x p)
-- on a layer with p neurons and m neurons in the previous layer
forwardLayer :: Layer -> Matrix R -> LayerState
forwardLayer layer@Layer {..} x =
    let z =  (x <> w) + b
        a =  (sigmoid . sigm $ spec) z
    in LayerState { .. }

forwardNetwork :: NeuralNetwork -> Matrix R -> Matrix R -> NetworkState
forwardNetwork NeuralNetwork { .. } xs ys = NetworkState xs theta layerStates
 where layerStates  = go xs unLayers
       go _ []      = []
       go x' (l:ls) =
           let st = forwardLayer l x'
           in st : go (a st) ls
       theta = let res = a (RU.last layerStates)
                   n   = fromIntegral $ M.rows ys
                   err = M.scale (1/n) . M.cmap (**2) $ ys - res
               in M.sumElements err

initializeLayer :: (PrimMonad m) => Gen (PrimState m) -> Int -> LayerSpec -> m Layer
initializeLayer rg m spec = do
    let n = size spec
    b  <- DG.standardNormalM rg 1 n
    w  <- DG.standardNormalM rg m n
    return Layer { .. }

initializeNetwork :: NetworkSpec -> IO NeuralNetwork
initializeNetwork NetworkSpec {..} = do
    rg       <- create
    let dims = inputDims : (size <$> layerSpecs)
    NeuralNetwork <$> zipWithM (initializeLayer rg) dims layerSpecs

-- update the weights in the layer according to the gradients stored
-- in the backprop structure.
updateLayer :: Layer -> LayerGradients -> Layer
updateLayer l g = l
    { w = w l - M.scale (alpha . spec $ l) (jw g)
    , b = b l - M.scale (alpha . spec $ l) (jb g) }

updateNetwork :: NeuralNetwork -> [LayerGradients] -> NeuralNetwork
updateNetwork nn gs = nn
    { unLayers = zipWith updateLayer (unLayers nn) gs }

train' :: Matrix R -> Matrix R -> NeuralNetwork -> [(NeuralNetwork, NetworkState)]
train' xs ys initialNetwork = iterate step (initialNetwork, undefined)
 where step (nn', _) = let ns = forwardNetwork nn' xs ys
                           bp = backprop ys ns
                       in  (updateNetwork nn' bp, ns)

train :: Matrix R -> Matrix R -> Int -> NeuralNetwork -> NeuralNetwork
train xs ys n = fst . RU.head . drop (n-1) . train' xs ys

iteration :: Matrix R -> Matrix R -> NeuralNetwork -> NeuralNetwork
iteration xs ys nn =
    let ns = forwardNetwork nn xs ys
        bp = backprop ys ns
    in  updateNetwork nn bp

epoch :: [VS.Vector R] -> [VS.Vector R] -> Int -> NeuralNetwork -> NeuralNetwork
epoch xs ys batchSize nn =
    let batches          = S.chunksOf batchSize xs `zip` S.chunksOf batchSize ys
        go nn' []                = nn'
        go nn' ((xs', ys'):rest) = go (iteration (M.fromRows xs') (M.fromRows ys') nn') rest
    in  go nn batches


result :: NetworkState -> Matrix R
result = a . RU.last . layerStates

numCorrect :: Eq a => VS.Storable a => VS.Vector a -> VS.Vector a -> Int
numCorrect v1 v2 = VS.sum $ VS.zipWith (\a b -> if a == b then 1 else 0) v1 v2

encodeOneHot :: Integral a => a -> VS.Vector Double
encodeOneHot hot = VS.generate 10 (\idx -> if idx == fromIntegral hot then 1 else 0)

decodeOneHot :: VS.Vector Double -> Int
decodeOneHot = fromIntegral . VS.maxIndex

printNetworkConfig :: NeuralNetwork -> String
printNetworkConfig NeuralNetwork {..} =
    let printLayer l = show (size $ spec l) ++ "xD [" ++ show (sigm $ spec l) ++
            "] @ " ++ show (alpha $ spec l)
    in intercalate "," $ printLayer <$> unLayers
