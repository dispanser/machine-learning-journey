{-# LANGUAGE RecordWildCards #-}

module ML.TenDaysOfGrad where

{-
    based on the blog series at http://penkovsky.com/neural-networks
-}

import qualified Relude.Unsafe as RU
import           Data.Vector.Storable (Vector)
-- import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra (Matrix, R)
import qualified Numeric.LinearAlgebra as M
import qualified ML.NN01 as NN

-- record to have names for the fields, and vector instead of matrix as type
-- for the bias, as it properly reflects its dimenions (1d).
data Layer a = Layer
    { w :: Matrix a
    , b :: Vector a
    , a :: NN.Activation }

-- currently not used. We're modelling our Sigmoid based on the approach from
-- the previous attempt as a pair of functions (activation and its derivative).
data Activation = Relu | Sigmoid | Tanh | Id

data Gradient a = Gradient
    { dw :: Matrix a
    , db :: Vector a }

data DS = DS
    { xs :: Matrix R
    , ys :: Matrix R }

updateLayer :: R -> Layer R -> Gradient R -> Layer R
updateLayer learnRate layer gradients = layer
    { w = w layer - M.scale learnRate (dw gradients)
    , b = b layer - M.scale learnRate (db gradients) }

optimize :: Double -> Int -> [Layer R] -> DS -> [Layer R]
optimize lr iterN net0 dataSet = RU.last $ take iterN $ iterate step net0
 where
   step net = zipWith (updateLayer lr) net dW
    where
      (_, dW)  = pass' net dataSet

pass' :: [Layer R] -> DS -> (Matrix R, [Gradient R])
pass' net DS { .. } = (pred, gradients)
 where
   (_, pred, gradients) = _pass xs net
   _pass inp [] = (undefined, inp, undefined)
