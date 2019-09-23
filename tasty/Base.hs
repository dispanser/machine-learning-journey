{-# LANGUAGE FlexibleContexts #-}

module Base where

import           ML.Dataset
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as VG
import           Test.Hspec

contFeat :: Text -> Vector Double -> Feature
contFeat name' xs = Feature (Continuous name' (0, 1)) [xs]

catFeat :: Text -> NonEmpty Text-> Feature
catFeat = createCategorical

shouldRoughlyEqual :: (Show a, Num a, Ord a, Fractional a) => a -> a -> IO ()
shouldRoughlyEqual actual expected = actual `shouldSatisfy` roughlyEqual expected

roughlyEqual :: (Num a, Ord a, Fractional a) => a -> a -> Bool
roughlyEqual expected actual =
    (0.01 > abs (expected - actual)) ||
        (1.01 > max expected actual / min expected actual)

checkVector :: VG.Vector v Double => v Double -> [Double] -> IO ()
checkVector xs y = do
    VG.length xs `shouldBe` length y
    zipWithM_ (\x ex -> x `shouldRoughlyEqual` ex) (VG.toList xs) y

-- TODO: improve for better error messages
checkList :: [Double] -> [Double] -> IO ()
checkList xs ys = (length xs `shouldBe` length ys) >> zipWithM_ shouldRoughlyEqual xs ys
