{-# LANGUAGE OverloadedLists #-}

module ML.Data.VectorSuite where

import           Test.Hspec
import           Test.Tasty.Hspec (Spec)
import qualified Data.Vector.Unboxed as V
import           ML.Data.Vector (replaceNAs)

spec_replaceNAs :: Spec
spec_replaceNAs =
    it "N/A values should be replaced by the variable mean" $ do
        let vs = V.fromList [-1, sqrt $ -1, 3]
        replaceNAs vs `shouldBe` [-1, 1, 3 :: Double]


