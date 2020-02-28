{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ML.Classification.KNearestClassifier where

import           Data.List (sort, sortOn, nub)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import           ML.Model (ModelR(..), ModelSpec, modelInfo)
import           ML.Common.KNN (KNearest, initKNN, findNearest)


data KNNClassifier a = KNNClassifier
    { knn     :: KNearest a
    , classes :: [a] }

type KNNModel a = ModelR (KNNClassifier a) a

-- instance Ord a => Classifier (KNNClassifier a) where
  -- classify KNNClassifier { .. }  _k _obs = undefined

createKNNClassifier :: Ord a => ModelSpec -> [V.Vector Double] -> V.Vector a -> KNNModel a
createKNNClassifier ms xs y =
    let modelInfo = initKNNClassifier xs y
        buildWith = ms
        cost v w  = if (v == w) then 0 else 1
        predictR  = undefined
    in ModelR { .. }

initKNNClassifier :: Ord a => [V.Vector Double] -> V.Vector a -> KNNClassifier a
initKNNClassifier xs y =
    let classes = nub $ V.toList y
        knn     = initKNN xs y
    in KNNClassifier { .. }

classifyKNN :: (GV.Vector v Double, Ord a) => KNNClassifier a -> Int -> v Double -> [(Double, a)]
classifyKNN KNNClassifier { .. }  k obs =
    let nearest = sort $ findNearest knn k obs
        count (x :| xs)  = (1 + fromIntegral (length xs), x)
        freq    = fmap count $ NE.group nearest
        num     = sum $ fst <$> freq :: Double
    in sortOn ((0-) . fst) $ fmap  (\(f, x) -> (f / num, x)) freq
