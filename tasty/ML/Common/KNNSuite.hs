{-# LANGUAGE OverloadedLists #-}

module ML.Common.KNNSuite where


import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           Data.List (transpose, sort)
import qualified Data.Vector as V
import           ML.Common.KNN (findNearest, initKNN)
import           ML.Classification.KNearestClassifier (initKNNClassifier, classifyKNN)

spec_NearestNeighbors :: Spec
spec_NearestNeighbors =
    describe "on a rectangular 10x10 grid" $ do
        let grid   = [(x'+y',[x', y']) | x' <- [0..10], y' <- [0..10]]
            y      = fst <$> grid  -- response is grid distance to origin
            xs     = transpose $ snd <$> grid
            knn    = initKNN (V.fromList <$> xs) $ V.fromList y
        it "should find single nearest" $
            findNearest knn 1 (V.fromList [3, 3]) `shouldBe` [6]
        it "should find five nearest on grid" $
            -- we should find our search point, plus the four adjacent points.
            sort (findNearest knn 5 $ V.fromList [7, 1]) `shouldBe` [7, 7, 8, 9, 9]
        it "should find far away origin" $ -- [(0, 0), (0, 1), (0, 2)]
            findNearest knn 3 (V.fromList [-13, -299]) `shouldBe` [0, 1, 2]

spec_ClassifyKNN :: Spec
spec_ClassifyKNN =
    describe "on a rectangular 10x10 grid with two classes" $ do
        let grid   = [(x' >= y',[x', y']) | x' <- [0..10], y' <- [0..10]]
            y      = fst <$> grid  -- decision boundary is x=y+eps
            xs     = transpose $ snd <$> grid
            knn    = initKNNClassifier (V.fromList <$> xs) $ V.fromList y
        it "correctly classifies to nearest sample for k=1" $
            classifyKNN knn 1 (V.fromList [0, 3]) `shouldBe` [(1, False)]
        it "correctly determines probabilities for k=5" $
            classifyKNN knn 5 (V.fromList [1, 1]) `shouldBe` [(0.6, True), (0.4, False)]



