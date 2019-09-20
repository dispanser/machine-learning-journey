{-# LANGUAGE OverloadedLists #-}

module ML.Data.VectorSuite where

import           Base
import           Test.Hspec
import           Test.Tasty.Hspec (Spec)
import qualified Data.Vector.Unboxed as V
import           ML.Data.Vector (replaceNAs, scaleWith, unscaleWith)
import           Test.Tasty.QuickCheck

spec_replaceNAs :: Spec
spec_replaceNAs =
    it "N/A values should be replaced by the variable mean" $ do
        let vs = V.fromList [-1, sqrt $ -1, 3]
        replaceNAs vs `shouldBe` [-1, 1, 3 :: Double]


-- prop_scaling :: NonEmptyList Double -> Double -> Positive Double -> Bool
prop_scaling :: [Double] -> Double -> Double -> Property
prop_scaling xs shift sc = length xs > 0 && sc /= 0 ==>
    let xs'     = V.fromList xs
        scaling = (shift, sc) :: (Double, Double)
    in checkVector (unscaleWith scaling $ scaleWith scaling xs') xs
