{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.LinearRegression where

import           ISL.DataSet (Summary(..))
import qualified ISL.Model as M
import           ISL.Model (ModelInput(..), Feature(..), Column(..))
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

data LinearRegression = LinearRegression
    { lrFeatureNames   :: [T.Text]
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

instance M.Predictor LinearRegression where
  predict LinearRegression { .. } xss =
      let input = VS.convert <$> concatMap M.featureVectors xss
          olsR  = predictLinearRegression lrCoefficients input
      in SingleCol . Column lrResponseName . VS.convert $ olsR

instance Summary LinearRegression where
  summary = summarizeLinearRegression

summarizeLinearRegression :: LinearRegression -> [T.Text]
summarizeLinearRegression lr@LinearRegression { .. }  =
    [ formatFormula lrResponseName lrFeatureNames
    , "Feature           | coefficient |  std error  |  t-stats |  p-value"
    , "------------------+-------------+-------------+----------+---------"
    ] ++
        (V.toList $ V.zipWith3 (formatCoefficientInfo $ fromIntegral lrDF)
            (V.fromList $ "Intercept" : lrFeatureNames)
            (V.convert lrCoefficients) (V.convert lrStandardErrors)) ++
        [ ""
        , F.sformat ("R^2         : " % F.fixed 4) lrR2
        , F.sformat ("F-Statistics: " % F.fixed 4) $ fStatistics lr]

-- format the features and response used for the regression, R-style
formatFormula :: T.Text -> [T.Text] -> T.Text
formatFormula responseName featureNames =
    F.sformat ("Linear Regression: " % F.stext % " ~ ") responseName
      <> T.intercalate " + " featureNames

-- format a coefficient into a nicely laid out string
formatCoefficientInfo :: Double -> T.Text -> Double -> Double -> T.Text
formatCoefficientInfo df name x err =
    let scieF         = F.left 11 ' ' %. F.scifmt Scientific.Exponent (Just 4)
        numF          = F.left 8 ' '  %. F.fixed 4
        formatString = (F.left 17 ' ' %. F.stext) % " | " % scieF % " | " % scieF %
            " | " % numF % " | " % numF
        tStat        = x / err
        -- TODO: multiplying by two gives the same values as R, but that's not
        -- a good reason to randomly multiply by something. Investigate!
        pV           = 2 * (pValue df $ abs tStat)
    in F.sformat formatString name
        (Scientific.fromFloatDigits x)
        (Scientific.fromFloatDigits err) tStat pV

instance M.ModelFit LinearRegression where
  fit = linearRegression

linearRegression :: ModelInput -> LinearRegression
linearRegression ModelInput { .. } =
    let y                = VS.convert $ fromMaybe V.empty $ listToMaybe $
                               M.featureVectors miResponse
        xs               = VS.convert <$> concatMap M.featureVectors miFeatures
        n                = VS.length y
        xX               = prepareMatrix n xs
        p                = M.cols xX - 1
        lrDF             = n - p - 1
        -- TODO: ugly as hell
        lrCoefficients   = fromMaybe VS.empty $ fst <$> (uncons $ M.toColumns $ M.linearSolveLS xX (M.fromColumns [y]))
        residuals        = y - predictLinearRegression lrCoefficients xs
        lrRss            = residuals <.> residuals
        yMean            = mean y
        yDelta           = y - (VS.replicate n yMean)
        lrTss            = yDelta <.> yDelta
        lrMse            = lrRss / fromIntegral (n - p - 1)
        lrRse            = sqrt lrMse
        lrStandardErrors = M.takeDiag $ M.scale lrMse (M.inv . M.unSym $ M.mTm xX) ** 0.5
        lrR2             = 1 - lrRss/lrTss
        lrResponseName   = M.featureName miResponse
        lrFeatureNames   = concatMap M.columnNames miFeatures
    in  LinearRegression { .. }


predictLinearRegression :: Vector Double -> [Vector Double] -> Vector Double
predictLinearRegression bs xs =
    let n  = fromMaybe 0 $ VS.length . fst <$> uncons xs
        xX = prepareMatrix n xs
    in if M.cols xX == VS.length bs
          then xX #> bs
          else error $ "number of input columns '" <>
                show (M.cols xX) <> "' does not match expected '" <>
                    show (VS.length bs) <> "'"

mean :: (VS.Storable a, Fractional a) => Vector a -> a
mean xs = VS.sum xs / (fromIntegral $ VS.length xs)

mse :: LinearRegression -> Double
mse LinearRegression { .. } = lrRss / fromIntegral n

tStatistics :: LinearRegression -> Vector Double
tStatistics LinearRegression { .. } = abs $ lrCoefficients / lrStandardErrors

fStatistics :: LinearRegression -> Double
fStatistics LinearRegression { .. } = (lrTss - lrRss) / fromIntegral p / lrRss * fromIntegral lrDF

pValue :: Double -> Double -> Double
pValue df v =
    let tDist = studentT df
    in complCumulative tDist v

-- transform a list of column vectors into a matrix, prepending a column of ones
-- for the intercept
prepareMatrix :: Int -> [Vector Double] -> Matrix R
prepareMatrix n xs =
    let m = M.fromColumns $ VS.replicate n 1 : xs
    in trace (show (M.rows m, M.cols m)) m
