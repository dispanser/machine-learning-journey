{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Vector
    ( replaceNAs
    , summarizeVector
    , parseNumbers
    , normalize
    , noScaling
    , scale01
    , scaleVector
    , scaleWith
    , unscaleWith
    , vmean ) where

import           ML.Data.Summary
import           Data.Either (fromRight)
import           Text.Read (readEither)
import           Data.Text (Text, unpack)
import qualified Data.Vector.Generic as VG
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Statistics.Function (minMax)
import qualified Statistics.Quantile as Q
import qualified Statistics.Sample as S

type Scaling           = (Double, Double)
type ScaleStrategy     = Vector Double -> Scaling

-- TODO: generic method?
replaceNAs :: Vector Double -> Vector Double
replaceNAs xs =
    let mean      = vmean $ V.filter (not . isNaN) xs
    in V.map (\x -> if isNaN x then mean else x) xs

vmean :: (Fractional a, VG.Vector v a) => v a -> a
vmean vs = VG.sum vs / fromIntegral (VG.length vs)

-- human-readable representation of a vector. Note that we unapply the scaling
-- process to present the data in its original input form.
summarizeVector :: Text -> Scaling -> Vector Double -> Text
summarizeVector name sc xsScaled =
    let xs = unscaleWith sc xsScaled
        [min', fstQ, med, thrdQ, max'] =
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
    in replaceNAs $ V.fromList $ fromRight fallback . readEither . unpack <$> xs

scale01 :: ScaleStrategy
scale01 xs =
    let (minV, maxV) = minMax xs
    in  (minV, maxV - minV)

normalize :: ScaleStrategy
normalize xs =
    let (mn, var) = S.meanVariance xs
    in (mn, sqrt var)

noScaling :: ScaleStrategy
noScaling = const (0, 1)

scaleVector :: ScaleStrategy -> Vector Double -> Vector Double
scaleVector sc xs = scaleWith (sc xs) xs

scaleWith :: Scaling -> Vector Double -> Vector Double
scaleWith (shift, scale) = V.map ((/scale) . subtract shift)

unscaleWith :: Scaling -> Vector Double -> Vector Double
unscaleWith (shift, scale) = V.map ((+ shift) . (*scale))
