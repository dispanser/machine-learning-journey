{-# LANGUAGE FlexibleContexts #-}

module Base where

import           ML.Dataset
import           Control.Monad (zipWithM_)
import           Data.Text (Text)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as VG
import           Test.Hspec

contFeat :: Text -> Vector Double -> Feature
contFeat name' xs = Feature (Continuous name' (0, 1)) [xs]

shouldRoughlyEqual :: (Show a, Num a, Ord a, Fractional a) => a -> a -> IO ()
shouldRoughlyEqual actual expected =
    if actual `roughlyEqual` expected
       then return ()
       else actual `shouldBe` expected

roughlyEqual :: (Num a, Ord a, Fractional a) => a -> a -> Bool
roughlyEqual expected actual =
    (0.01 > abs (expected - actual)) ||
        (1.01 > max expected actual / min expected actual)

checkVector :: VG.Vector v Double => v Double -> [Double] -> IO ()
checkVector xs y = do
    VG.length xs `shouldBe` length y
    VG.zipWithM_ shouldRoughlyEqual xs $ VG.fromList y

-- TODO: improve for better error messages
checkList :: [Double] -> [Double] -> IO ()
checkList xs ys = (length xs `shouldBe` length ys) >> zipWithM_ shouldRoughlyEqual xs ys

isLeft :: Eq a => Show a => Either a b -> a -> Expectation
isLeft (Left a) expected  = a `shouldBe` expected
isLeft (Right _) expected = fail $ "got right, but expected Left" ++ show expected
