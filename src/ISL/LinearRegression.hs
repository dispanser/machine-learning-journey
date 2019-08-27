{-# LANGUAGE RecordWildCards #-}

module ISL.LinearRegression where

import           ISL.DataSet
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R, (#>), (<.>))
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable (Vector)
import           Statistics.Distribution.StudentT (studentT)
import           Statistics.Distribution (complCumulative)

import Debug.Trace (trace)

data LinearRegression = LinearRegression
    { lrCoefficients   :: Vector Double
    , lrStandardErrors :: Vector Double
    , lrRse            :: Double
    , lrR2             :: Double
    , lrTss            :: Double
    , lrRss            :: Double
    , n                :: Int
    , p                :: Int
    } deriving (Show, Eq, Ord)

instance Predictor LinearRegression where
  predict LinearRegression { .. } xss =
      V.convert $ predictLinearRegression lrCoefficients (V.convert <$> xss)

instance Summary LinearRegression where
  summary LinearRegression { .. } =
      unlines [ "-- Linear Regression --" ]

linearRegression :: [Vector Double] -> Vector Double -> LinearRegression
linearRegression xs y =
    let n                = V.length y
        xX               = prepareMatrix n xs
        p                = M.cols xX - 1
        lrCoefficients   = head $ M.toColumns $ M.linearSolveLS xX (M.fromColumns [y])
        residuals        = y - predictLinearRegression lrCoefficients xs
        lrRss            = residuals <.> residuals
        yMean            = mean y
        yDelta           = y - (V.replicate n yMean)
        lrTss            = yDelta <.> yDelta
        lrMse            = lrRss / fromIntegral (n - p - 1)
        lrRse            = sqrt lrMse
        lrStandardErrors = M.takeDiag $ M.scale lrMse (M.inv . M.unSym $ M.mTm xX) ** 0.5
        lrR2             = 1 - lrRss/lrTss
    in  LinearRegression { .. }

predictLinearRegression :: Vector Double -> [Vector Double] -> Vector Double
predictLinearRegression bs xs =
    let n  = V.length $ head xs
        xX = prepareMatrix n xs
    in xX #> bs

mean :: (V.Storable a, Fractional a) => Vector a -> a
mean xs = V.sum xs / (fromIntegral $ V.length xs)

mse :: LinearRegression -> Double
mse LinearRegression { .. } = lrRss / fromIntegral n

tStatistics :: LinearRegression -> Vector Double
tStatistics LinearRegression { .. } = lrCoefficients / lrStandardErrors

fStatistics :: LinearRegression -> Double
fStatistics LinearRegression { .. } = (lrTss - lrRss) / fromIntegral p / lrRss * fromIntegral (n - p - 1)

pValue :: Double -> Double -> Double
pValue df v =
    let tDist = studentT df
    in complCumulative tDist v

-- transform a list of column vectors into a matrix, prepending a column of ones
-- for the intercept
prepareMatrix :: Int -> [Vector Double] -> Matrix R
prepareMatrix n xs = M.fromColumns $ V.replicate n 1 : xs

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v
