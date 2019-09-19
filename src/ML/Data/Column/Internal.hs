{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Column.Internal
    ( RowSelector
    , ColumnTransformer
    , Column(..)
    , filterDataColumn
    , scaleColumn
    , scaleVector
    , scale01
    , vmean
    , columnVariance
    , mkColumn
    , summarizeVector
    ) where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as VG
import           Statistics.Function (minMax)
import qualified Statistics.Quantile as Q
import qualified Statistics.Sample as S
import           ML.Data.Summary

type RowSelector       = Int           -> Bool
type ColumnTransformer = Vector Double -> Vector Double
type Scaling           = (Double, Double)
type ScaleStrategy     = Vector Double -> Scaling

data Column a = Column
    { colName :: Text
    , scale   :: Double
    , shift   :: Double
    , colData :: Vector a } deriving (Eq, Show)

instance Summary (Column Double) where
  summary = (:[]) . summarizeColumn

-- type TransformRule  = VB.Vector Text           -> [Column Double]

-- handles the ingestion of a single feature, transforming the text values
-- into a numerical representation.
-- type FeatureParser  = VB.Vector Text -> ([Column Double], TransformRule)

mkColumn :: Text -> Vector a -> Column a
mkColumn n xs = Column n 1 0 xs

summarizeColumn :: Column Double -> Text
summarizeColumn Column { .. } = summarizeVector colName colData

summarizeVector :: Text -> Vector Double -> Text
summarizeVector name xs =
    let [min', fstQ, med, thrdQ, max'] =
            Q.quantiles Q.medianUnbiased [0..4] 4 xs
        mean = vmean xs
    in sformat (textF  13 % " Min: " % scieF % " 1stQ:" % scieF %
        " Med: " % scieF % " 3rdQ:" % scieF % " Max:" % scieF %
            " Mean:" % scieF)
            name (dSc min') (dSc fstQ) (dSc med) (dSc thrdQ)
            (dSc max') (dSc mean)

scaleColumn :: ScaleStrategy -> Column Double -> Column Double
scaleColumn sc col =
    let (range, shift) = sc $ colData col
        transF         = ((/range) . subtract shift)
    in Column
        { colName = colName col
        , scale   = range   -- 1/range, to be pedantic
        , shift   = shift  -- -minV, to be pedantic
        , colData = V.map transF $ colData col }

scale01 :: VG.Vector v Double => v Double -> (Double, Double)
scale01 xs =
    let (minV, maxV) = minMax xs
    in  (maxV - minV, minV)

scaleVector :: ScaleStrategy -> Vector Double -> Vector Double
scaleVector sc xs =
    let (range, shift) = sc xs
        transF         = ((/range) . subtract shift)
    in V.map transF xs

filterDataColumn :: V.Unbox a => RowSelector -> Vector a -> Vector a
filterDataColumn rs xs =V.ifilter (\i _ -> rs i) xs

vmean :: VG.Vector v Double => v Double -> Double
vmean vs = VG.sum vs / fromIntegral (VG.length vs)

columnVariance :: Vector Double -> Double
columnVariance = S.varianceUnbiased

columnLength :: V.Unbox a => [Column a] -> Int
columnLength []     = 0
columnLength (x:_) = V.length . colData $ x

