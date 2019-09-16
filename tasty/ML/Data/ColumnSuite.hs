module ML.Data.ColumnSuite where

import qualified Data.List as DL
import qualified Data.Vector.Unboxed as V
import           Test.QuickCheck
import ML.Data.Column.Internal

prop_Rescaled :: NonEmptyList Double -> Property
prop_Rescaled (NonEmpty xs) = (DL.maximum xs /= DL.minimum xs) ==>
    let vs   = rescale01 $ V.fromList xs
        minV = V.minimum vs
        maxV = V.maximum vs
    in minV >= 0 && maxV <= 1
