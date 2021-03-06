{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ML.LinearRegression where

import qualified ML.Model as M
import           ML.Dataset (Feature(..), Metadata(..), knownFeats)
import qualified ML.Data.Feature.Internal as F
import qualified ML.Dataset as DS
import           ML.Model (ModelSpec(..), ModelInit(..))
import           ML.Data.Summary
import           ML.Data.Vector (vmean)
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, R, (#>), (<.>))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Scientific as Scientific
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (Vector)
import           Statistics.Distribution.StudentT (studentT)
import           Statistics.Distribution (complCumulative)

data LinearRegression = LinearRegression
    { lrCoefficients   :: Vector Double
    , lrStandardErrors :: Vector Double
    , lrTss            :: Double
    , lrRss            :: Double
    , lrDF             :: Int
    , n                :: Int
    , p                :: Int
    , lrModelSpec      :: ModelSpec
    }

-- not everything is actually restricted to linear model, but that's all
-- we got for now
class LinearModel a where
  coefficients    :: a -> Vector Double
  rss             :: a -> Double
  tss             :: a -> Double
  degreesOfFredom :: a -> Double

instance LinearModel LinearRegression where
  coefficients    = lrCoefficients
  rss             = lrRss
  tss             = lrTss
  degreesOfFredom = fromIntegral . lrDF

instance M.Model LinearRegression where
  modelSpec' = lrModelSpec

instance M.Predictor LinearRegression where
  predict   LinearRegression { .. } cols  =
      Feature (response lrModelSpec) [VS.convert $
          predictLinearRegression lrCoefficients $ VS.convert <$> cols]

instance Summary LinearRegression where
  summary = summarizeLinearRegression

summarizeLinearRegression :: LinearRegression -> [Text]
summarizeLinearRegression lr@LinearRegression { .. }  =
    let featureNames = DS.columnNames $ features' lrModelSpec
    in [ formatFormula
        (M.responseName lrModelSpec)
        featureNames
       , "Feature           | coefficient |  std error  |  t-stats |  p-value"
       , "------------------+-------------+-------------+----------+---------"
       ] ++
           (V.toList $ V.zipWith3 (formatCoefficientInfo $ fromIntegral lrDF)
            (V.fromList $ "Intercept" : featureNames)
            (V.convert lrCoefficients) (V.convert lrStandardErrors)) ++
                [ ""
                , sformat ("R^2         : " % fixed 4) (r2 lr)
                , sformat ("F-Statistics: " % fixed 4) $ fStatistics lr]

-- format the features and response used for the regression, R-style
formatFormula :: Text -> [Text] -> Text
formatFormula responseName featureNames =
    sformat ("Linear Regression: " % stext % " ~ ") responseName
      <> T.intercalate " + " featureNames

-- format a coefficient into a nicely laid out string
formatCoefficientInfo :: Double -> Text -> Double -> Double -> Text
formatCoefficientInfo df name x err =
    let numF'          = left 8 ' '  %. fixed 4
        formatString = (left 17 ' ' %. stext) % " | " % scieF % " | " % scieF %
            " | " % numF' % " | " % numF'
        tStat        = x / err
        -- TODO: multiplying by two gives the same values as R, but that's not
        -- a good reason to randomly multiply by something. Investigate!
        pV           = 2 * (pValue df $ abs tStat)
    in sformat formatString name
        (Scientific.fromFloatDigits x)
        (Scientific.fromFloatDigits err) tStat pV

fitLinearRegression :: ModelSpec -> M.ModelInit LinearRegression
fitLinearRegression ms = M.ModelInit
    { fitF      = fitLR ms
    , modelSpec = ms }

fitLR :: ModelSpec -> M.FitF LinearRegression
fitLR ms inputCols response =
    let y  = VS.convert response
        n  = VS.length y
        (removedCols, featureCols) = L.partition
            ((==0.0) . DS.columnVariance . snd) $
                zip (DS.columnNames $ features' ms) inputCols
        xs = VS.convert . snd <$> featureCols
        xX = prepareMatrix n xs
        p  = pred $ M.cols xX
        lrDF             = n - p - 1
        -- TODO: ugly as hell
        lrCoefficients   = fromMaybe VS.empty $ listToMaybe $
            M.toColumns $ M.linearSolveLS xX $ M.fromColumns [y]
        residuals        = y - xX #> lrCoefficients
        lrRss            = residuals <.> residuals
        yMean            = vmean y
        yDelta           = y - (VS.replicate n yMean)
        lrTss            = yDelta <.> yDelta
        mse'             = lrRss / fromIntegral lrDF
        lrStandardErrors = M.takeDiag $ M.scale mse' (
            M.inv . M.unSym $ M.mTm xX) ** 0.5
        fs'              = foldl' removeColumn (features' ms) (fst <$> removedCols)
        lrModelSpec      = ms { features' =  fs' }
    in  LinearRegression { .. }

-- remove a dropped column from the feature space,
-- TODO: this screams lenses, updating some inner struct by throwing out stuff
removeColumn :: DS.FeatureSpace -> Text -> DS.FeatureSpace
removeColumn fs colName = fromMaybe fs $ removeFeature fs colName <|> removeLabel fs colName

removeFeature :: DS.FeatureSpace -> Text -> Maybe DS.FeatureSpace
removeFeature fs featName = do
    feat <- DS.findFeature fs featName
    return $ fs { knownFeats = filter (/= feat) $ knownFeats fs }



-- remove a label from the metadata of a feature space, to hide it from
-- being used. This is relevant when filtering out constant columns to
-- prevent singular matrices etc.
-- TODO: this screams lenses, updating some inner struct by throwing out stuff
removeLabel :: DS.FeatureSpace -> Text -> Maybe DS.FeatureSpace
removeLabel fs colName = do
    (f, l)   <- DS.splitFeatureName colName
    c@Categorical {..} <- DS.findFeature fs f
    let newMeta = c { otherLabels = (/= l) `filter` otherLabels }
    let others  = filter (/= c) $ knownFeats fs
    return $ DS.createFeatureSpace (newMeta:others)

predictLinearRegression :: Vector Double -> [Vector Double] -> Vector Double
predictLinearRegression bs xs =
    let n  = fromMaybe 0 $ VS.length . fst <$> uncons xs
        xX = prepareMatrix n xs
    in if M.cols xX == VS.length bs
          then xX #> bs
          else error $ "number of input columns '" <>
                show (M.cols xX) <> "' does not match expected '" <>
                    show (VS.length bs) <> "'"

tStatistics :: LinearRegression -> Vector Double
tStatistics LinearRegression { .. } = abs $ lrCoefficients / lrStandardErrors

fStatistics :: LinearModel a => a -> Double
fStatistics m =
    (tss m - rss m) / (fromIntegral . pred . VS.length $ coefficients m)
        / rss m * degreesOfFredom m

mse :: (M.Model a, LinearModel a) => a -> Double
mse m =
    let respScale = snd $ scalingFactor $ response $ M.modelSpec' m
    in rss m * respScale^(2::Int) / degreesOfFredom m

rse :: (M.Model a, LinearModel a) => a -> Double
rse = sqrt . mse

r2 :: (M.Model a, LinearModel a) => a -> Double
r2 m = 1 - rss m / tss m

pValue :: Double -> Double -> Double
pValue df v =
    let tDist = studentT df
    in complCumulative tDist v

-- transform a list of column vectors into a matrix, prepending a column of ones
-- for the intercept
prepareMatrix :: Int -> [Vector Double] -> Matrix R
prepareMatrix n xs = M.fromColumns $ VS.replicate n 1 : xs

-- metadata, replicated to have one metadata entry per column
--
metadataStream :: DS.FeatureSpace -> [Metadata]
metadataStream (knownFeats -> mds) =
    concatMap (\md -> replicate (columnsForFeature md) md) mds

-- given some metadata, how many columns of actual data is it representing?
columnsForFeature :: Metadata -> Int
columnsForFeature (Continuous _ _)     = 1
columnsForFeature (Categorical _ _ ol) = length ol

scalingFactor :: Metadata -> F.Scaling
scalingFactor (Continuous _ sc)   = sc
scalingFactor (Categorical _ _ _) = (0, 1)

recoverOriginalCoefficients :: (M.Model a, LinearModel a) => a -> [Double]
recoverOriginalCoefficients m =
    let ms = M.modelSpec' m
        fs = features' ms
        rs = response ms
        Just (intercept, regularCoefficients) = uncons $ VS.toList $ coefficients m
        (rShift, rScale)   = scaling rs
        scalingFactors     = scalingFactor <$> metadataStream fs
        scaledCoefficients = zipWith (\c (_, sc) -> c * rScale / sc)
            regularCoefficients scalingFactors
        intercept'  = intercept * rScale + rShift
        interceptShifts = zipWith (\c (sh, _) -> c * sh) scaledCoefficients scalingFactors
        intercept'' = intercept' - sum interceptShifts
    in intercept'' : scaledCoefficients
