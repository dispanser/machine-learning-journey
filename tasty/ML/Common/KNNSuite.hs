{-# LANGUAGE OverloadedLists #-}

module ML.Common.KNNSuite where


import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           ML.Common.KNN (findNearest, initKNN)

spec_NearestNeighbors :: Spec
spec_NearestNeighbors = do
    describe "on a rectangular 10x10 grid" $ do
        let grid   = [[x'+y',x', y'] | x' <- [0..10], y' <- [0..10]]
            (y:xs) = transpose grid
            knn    = initKNN (fromList <$> xs) $ fromList y
        it "should find single nearest" $
            findNearest knn 1 [3, 3] `shouldBe` [6]
        it "should find five nearest on grid" $
            -- we should find our search point, plus the four adjacent points.
            sort (findNearest knn 5 [7, 1]) `shouldBe` [7, 7, 8, 9, 9]
        it "should find far away origin" $
            findNearest knn 3 [-13, -299] `shouldBe` [0, 1, 2]


