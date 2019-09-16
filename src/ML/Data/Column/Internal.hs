{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Column.Internal
    ( RowSelector
    , ColumnTransformer
    , Column(..)
    , ScaledColumn(..)
    , filterDataColumn
    , rescaleColumn
    , rescale01
    , vmean
    , columnLength
    , columnVariance
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

data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Eq, Show)

instance Summary (Column Double) where
  summary = (:[]) . summarizeColumn

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

rescaleColumn :: Column Double -> ScaledColumn
rescaleColumn c =
    let (minV, maxV) = minMax $ colData c
        range        = maxV - minV
    in ScaledColumn
        { rawColumn   = c { colData =  VG.map (\x -> (x - minV) / range) $ colData c}
        , scaleOffset = minV   -- -minV, to be pedantic
        , scaleFactor = range  -- 1/range, to be pedantic
        }

data ScaledColumn  = ScaledColumn
    { rawColumn   :: Column Double
    , scaleOffset :: Double
    , scaleFactor :: Double
    } deriving Show

rescale01 :: VG.Vector v Double => v Double -> v Double
rescale01 xs =
    let (minV, maxV) = minMax xs
        range        = maxV - minV
    in VG.map (\x -> (x - minV) / range) xs

filterDataColumn :: V.Unbox a => RowSelector -> Column a -> Column a
filterDataColumn rs (Column name cData) =
    Column name $ V.ifilter (\i _ -> rs i) cData

vmean :: VG.Vector v Double => v Double -> Double
vmean vs = VG.sum vs / fromIntegral (VG.length vs)

columnVariance :: Column Double -> Double
columnVariance = S.varianceUnbiased . colData

columnLength :: V.Unbox a => [Column a] -> Int
columnLength []     = 0
columnLength (x:_) = V.length . colData $ x
