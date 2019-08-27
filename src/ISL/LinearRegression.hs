{-# LANGUAGE RecordWildCards #-}

module ISL.LinearRegression where

import qualified ISL.DataSet as DS
import           ISL.DataSet (ModelInput(..), Column(..))
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R, (#>), (<.>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (Vector)
import           Statistics.Distribution.StudentT (studentT)
import           Statistics.Distribution (complCumulative)

import Debug.Trace (trace)

data LinearRegression = LinearRegression
    { lrFeatureNames   :: V.Vector T.Text
    , lrResponseName   :: T.Text
    , lrCoefficients   :: Vector Double
    , lrStandardErrors :: Vector Double
    , lrRse            :: Double
    , lrR2             :: Double
    , lrTss            :: Double
    , lrRss            :: Double
    , n                :: Int
    , p                :: Int
    } deriving (Show, Eq, Ord)

instance DS.Predictor LinearRegression where
  predict LinearRegression { .. } xss =
      VS.convert $ predictLinearRegression lrCoefficients (VS.convert <$> xss)

instance DS.Summary LinearRegression where
  summary LinearRegression { .. } =
      unlines [ "-- Linear Regression --" ]

instance DS.ModelFit LinearRegression where
  fit = linearRegression

linearRegression :: ModelInput -> LinearRegression
linearRegression ModelInput { .. } =
    let y                = VS.convert $ colData miResponse :: VS.Vector Double
        xs               = VS.convert . colData <$> miFeatures
        n                = VS.length y
        xX               = prepareMatrix n xs
        p                = M.cols xX - 1
        lrCoefficients   = head $ M.toColumns $ M.linearSolveLS xX (M.fromColumns [y])
        residuals        = y - predictLinearRegression lrCoefficients xs
        lrRss            = residuals <.> residuals
        yMean            = mean y
        yDelta           = y - (VS.replicate n yMean)
        lrTss            = yDelta <.> yDelta
        lrMse            = lrRss / fromIntegral (n - p - 1)
        lrRse            = sqrt lrMse
        lrStandardErrors = M.takeDiag $ M.scale lrMse (M.inv . M.unSym $ M.mTm xX) ** 0.5
        lrR2             = 1 - lrRss/lrTss
        lrResponseName   = colName miResponse
        lrFeatureNames   = V.fromList $ colName <$> miFeatures
    in  LinearRegression { .. }


predictLinearRegression :: Vector Double -> [Vector Double] -> Vector Double
predictLinearRegression bs xs =
    let n  = VS.length $ head xs
        xX = prepareMatrix n xs
    in xX #> bs

mean :: (VS.Storable a, Fractional a) => Vector a -> a
mean xs = VS.sum xs / (fromIntegral $ VS.length xs)

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
prepareMatrix n xs = M.fromColumns $ VS.replicate n 1 : xs

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v
