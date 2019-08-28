{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.LinearRegression where

import qualified ISL.DataSet as DS
import           ISL.DataSet (ModelInput(..), Column(..))
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R, (#>), (<.>))
import qualified Formatting as F
import           Formatting ((%), (%.))
import qualified Data.Text as T
import qualified Data.Scientific as Scientific
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
    , lrDF             :: Int
    , n                :: Int
    , p                :: Int
    } deriving (Show, Eq, Ord)

instance DS.Predictor LinearRegression where
  predict LinearRegression { .. } xss =
      Column lrResponseName $
          VS.convert $ predictLinearRegression lrCoefficients (VS.convert . colData <$> xss)

instance DS.Summary LinearRegression where
  summary = summarizeLinearRegression

summarizeLinearRegression :: LinearRegression -> T.Text
summarizeLinearRegression lr@LinearRegression { .. }  = T.unlines $
    [ formatFormula lrResponseName $ V.toList lrFeatureNames
    , "Feature      | coefficient |  std error  |  t-stats |  p-value"
    , "-------------+-------------+-------------+----------+---------"
    ] ++
        (V.toList $ V.zipWith3 (formatCoefficientInfo $ fromIntegral lrDF) lrFeatureNames
            (V.convert lrCoefficients) (V.convert lrStandardErrors)) ++
        [ ""
        , F.sformat ("R^2         : " % F.fixed 4) lrR2
        , F.sformat ("F-Statistics: " % F.fixed 4) $ fStatistics lr]

-- format the features and response used for the regression, R-style
formatFormula :: T.Text -> [T.Text] -> T.Text
formatFormula responseName featureNames =
    F.sformat ("Linear Regression: " % F.stext % " ~ ") responseName
      <> head featureNames <> mconcat (F.sformat (" + " % F.stext) <$> tail featureNames)

-- format a coefficient into a nicely laid out string
formatCoefficientInfo :: Double -> T.Text -> Double -> Double -> T.Text
formatCoefficientInfo df name x err =
    let scieF         = F.left 11 ' ' %. F.scifmt Scientific.Exponent (Just 4)
        numF          = F.left 8 ' '  %. F.fixed 4
        formatString = (F.left 12 ' ' %. F.stext) % " | " % scieF % " | " % scieF %
            " | " % numF % " | " % numF
        tStat        = x / err
        -- TODO: multiplying by two gives the same values as R, but that's not
        -- a good reason to randomly multiply by something. Investigate!
        pV           = 2 * (pValue df $ abs tStat)
    in F.sformat formatString name
        (Scientific.fromFloatDigits x)
        (Scientific.fromFloatDigits err) tStat pV

instance DS.ModelFit LinearRegression where
  fit = linearRegression

linearRegression :: ModelInput -> LinearRegression
linearRegression ModelInput { .. } =
    let y                = VS.convert $ colData miResponse :: VS.Vector Double
        xs               = VS.convert . colData <$> miFeatures
        n                = VS.length y
        xX               = prepareMatrix n xs
        p                = M.cols xX - 1
        lrDF             = n - p - 1
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
        lrFeatureNames   = V.fromList $ "Intercept" : (colName <$> miFeatures)
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
tStatistics LinearRegression { .. } = abs $ lrCoefficients / lrStandardErrors

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
