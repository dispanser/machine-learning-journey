{-# LANGUAGE RecordWildCards #-}
{-
   very basic implementation of neural networks, based on 10 Days of Grad, Day 1
-}

module ML.NN01 where

import qualified Relude.Unsafe as RU
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified ML.Data.Generate as DG
import           Numeric.LinearAlgebra (Matrix, R)
import qualified Numeric.LinearAlgebra as M
import           Numeric.Morpheus.MatrixReduce(columnSum)
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
    , sigm  :: Sigmoid
    , alpha :: R } deriving Show
-- network spec: number of inputs, number of outputs, and n numbers
-- representing n hidden layers w/ n potentially differnt sigmoids
data NetworkSpec   = NetworkSpec
    { inputDims  :: Int
    , layerSpecs :: [LayerSpec] } deriving Show

data Sigmoid = ReLu | Tanh deriving (Show, Eq)

sigmoid :: Sigmoid -> R -> R
sigmoid Tanh = tanh
sigmoid ReLu = max 0

sigmoid' :: Sigmoid -> R -> R
sigmoid' Tanh x = (1/cosh x) ** 2
sigmoid' ReLu x = if x < 0 then 0 else 1

backprop :: Matrix R -> NetworkState -> [LayerGradients]
backprop ys NetworkState { .. } = backprop' a0 layerStates
 where
    backprop' :: Matrix R -> [LayerState] -> [LayerGradients]
    backprop' _  []               = []
    backprop' xs [LayerState{..}] =
        let dCda  = M.scale 2 (a - ys) -- n x p - n x p -> n x p
            dadz  = M.cmap (sigmoid' . sigm . spec $ layer) z -- n x p -> n x p
            dCdz' =  dCda * dadz
        in [ LayerGradients { jw   = M.tr xs <> dCdz'
                            , jb   = M.asRow $ columnSum dCdz'
                            , dCdz = dCdz' } ]
    backprop' xs (l:l':ls) =
        let bps@(lastBS:_) = backprop' (a l) $ l':ls
            sg'            = sigmoid' . sigm . spec . layer $ l
            dadz           = M.cmap sg' $ z l
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
        a =  M.cmap (sigmoid . sigm $ spec) z
    in LayerState { .. }

forwardNetwork :: NeuralNetwork -> Matrix R -> Matrix R -> NetworkState
forwardNetwork NeuralNetwork { .. } xs ys = NetworkState xs theta layerStates
 where layerStates  = go xs unLayers
       go _ []      = []
       go x' (l:ls) =
           let st = forwardLayer l x'
           in st : go (a st) ls
       theta = let res = a (RU.last layerStates)
                   err = M.cmap (**2) $ ys - res
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
train' xs ys nn = iterate step (nn, undefined)
 where step (nn, _) = let ns  = forwardNetwork nn xs ys
                          bp  = backprop ys ns
                      in  (updateNetwork nn bp, ns)

train :: Matrix R -> Matrix R -> Int -> NeuralNetwork -> NeuralNetwork
train xs ys n = fst . RU.head . drop (n-1) . train' xs ys

