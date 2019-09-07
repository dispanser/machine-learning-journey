{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ISL.DataSetSuite where

import           ML.DataSet
import           ISL.Model
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import qualified Data.Vector as V

spec_replaceNAs :: Spec
spec_replaceNAs =
    it "N/A values should be replaced by the variable mean" $ do
        let vs = V.fromList [-1, sqrt $ -1, 3]
        replaceNAs vs `shouldBe` V.fromList [-1, 1, 3 :: Double]

spec_splitVector :: Spec
spec_splitVector =
    describe "splitting a vector into two" $ do
        it "should handle empty vectors" $ do
            let (left, right) = splitVector (repeat False) (V.empty :: V.Vector Bool)
            left `shouldBe` V.empty
            right `shouldBe` V.empty
        it "should handle empty row selector" $ do
            let inp           = V.fromList [3, 4, 5, 6 :: Int]
                (left, right) = splitVector (repeat False) inp
            left `shouldBe` V.empty
            right `shouldBe` inp
        it "should handle full row selector" $ do
            let inp           = V.fromList [3 .. 7 :: Int]
                (left, right) = splitVector (repeat True) inp
            left `shouldBe` inp
            right `shouldBe` V.empty

