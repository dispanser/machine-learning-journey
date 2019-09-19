{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Vector
    ( replaceNAs
    , summarizeVector
    , parseNumbers
    , noScaling
    , scale01
    , scaleVector
    , scaleWith
    , vmean ) where

import           ML.Data.Summary
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Statistics.Function (minMax)
import qualified Statistics.Quantile as Q

type Scaling           = (Double, Double)
type ScaleStrategy     = Vector Double -> Scaling

-- TODO: generic method?
replaceNAs :: Vector Double -> Vector Double
replaceNAs xs =
    let mean      = vmean $ V.filter (not . isNaN) xs
    in V.map (\x -> if isNaN x then mean else x) xs

vmean :: (Fractional a, VG.Vector v a) => v a -> a
vmean vs = VG.sum vs / fromIntegral (VG.length vs)

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

parseNumbers :: [Text] -> V.Vector Double
parseNumbers xs =
    let fallback = sqrt $ -1
    in replaceNAs $ V.fromList $ fromRight fallback . readEither <$> xs

scale01 :: ScaleStrategy
scale01 xs =
    let (minV, maxV) = minMax xs
    in  (minV, maxV - minV)

noScaling :: ScaleStrategy
noScaling = const (0, 1)

scaleVector :: ScaleStrategy -> Vector Double -> Vector Double
scaleVector sc xs = scaleWith (sc xs) xs

scaleWith :: Scaling -> Vector Double -> Vector Double
scaleWith (shift, scale) = V.map ((/scale) . subtract shift)
