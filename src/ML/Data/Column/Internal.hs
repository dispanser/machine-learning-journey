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
    , scale01
    , vmean
    , columnLength
    , columnVariance
    , mkColumn
    ) where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as VG
import           Statistics.Function (minMax)
import qualified Statistics.Quantile as Q
import qualified Statistics.Sample as S
import           ML.Data.Summary

type RowSelector       = Int           -> Bool
type ColumnTransformer = Column Double -> Column Double
type Scaling           = (Double, Double)

data Column a = Column
    { colName :: Text
    , scale   :: Double
    , shift   :: Double
    , colData :: Vector a } deriving (Eq, Show)

instance Summary (Column Double) where
  summary = (:[]) . summarizeColumn

mkColumn :: Text -> Vector a -> Column a
mkColumn n xs = Column n 1 0 xs

summarizeColumn :: Column Double -> Text
summarizeColumn Column { .. } =
    let [min', fstQ, med, thrdQ, max'] =
            Q.quantiles Q.medianUnbiased [0..4] 4 colData
        mean = vmean colData
    in sformat (textF  13 % " Min: " % scieF % " 1stQ:" % scieF %
        " Med: " % scieF % " 3rdQ:" % scieF % " Max:" % scieF %
            " Mean:" % scieF)
            colName (dSc min') (dSc fstQ) (dSc med) (dSc thrdQ)
            (dSc max') (dSc mean)

scaleColumn :: (Vector Double -> Scaling) -> Column Double -> Column Double
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

filterDataColumn :: V.Unbox a => RowSelector -> Column a -> Column a
filterDataColumn rs (Column name sc sh cData) =
    Column name sc sh $ V.ifilter (\i _ -> rs i) cData

vmean :: VG.Vector v Double => v Double -> Double
vmean vs = VG.sum vs / fromIntegral (VG.length vs)

columnVariance :: Column Double -> Double
columnVariance = S.varianceUnbiased . colData

columnLength :: V.Unbox a => [Column a] -> Int
columnLength []     = 0
columnLength (x:_) = V.length . colData $ x

