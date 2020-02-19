module Prelude
       ( module Relude
       , Pred
       , debugShow
       , traceSome
       , matrixDim
       , everyNth
       ) where

import Relude
import qualified Relude.Unsafe as RU
import qualified Debug.Trace as D
import qualified Data.List.Split as S
import qualified Numeric.LinearAlgebra as M

debugShow :: (Show a, Show b) => b -> a -> a
debugShow prefix v =
    let msg = show prefix <> ": " <> show v
    in D.trace msg v

matrixDim :: Show a => a -> M.Matrix Double -> M.Matrix Double
matrixDim prefix mat =
    let m   = M.rows mat
        n   = M.cols mat
        msg = show prefix ++ " ∈ R^" ++ show m ++ "✖" ++ show n
    in myTrace msg mat

traceSome :: Show a => Show b => String -> (a -> b) -> a -> a
traceSome prefix f x =
    let msg = show prefix <> ": " <> show (f x)
    in myTrace msg x

myTrace :: String -> a -> a
-- myTrace = flip const
myTrace = D.trace

type Pred a = a -> Bool

everyNth :: Int -> [a] -> [a]
everyNth n xs = RU.last <$> S.chunksOf n xs
