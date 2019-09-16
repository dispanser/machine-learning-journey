{-# LANGUAGE OverloadedStrings #-}

module ML.Data.ColumnSuite where

import qualified Data.List as DL
import qualified Data.Vector.Unboxed as V
import           Test.QuickCheck
import ML.Data.Column.Internal

prop_Rescaled :: NonEmptyList Double -> Property
prop_Rescaled (NonEmpty xs) = (DL.maximum xs /= DL.minimum xs) ==>
    let vs   = scaleColumn scale01 $ mkColumn "test" $ V.fromList xs
        minV = V.minimum $ colData vs
        maxV = V.maximum $ colData vs
    in minV >= 0 && maxV <= 1
