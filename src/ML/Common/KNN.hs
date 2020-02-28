{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module ML.Common.KNN where

import qualified Relude.Unsafe as RU
import           Data.List (sortOn)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra (Matrix, R, fromColumns, fromRows, pairwiseD2, toColumns)

-- the type of the response values is kept generic, to be able to support
-- quantitative values for regression and qualitative values for classification.
data KNearest a = KNearest
    { knnX :: Matrix R
    , knnY :: V.Vector a
    }

-- find the k nearest neighbors for a single observation
findNearest :: GV.Vector v R => KNearest a -> Int -> v R -> [a]
findNearest KNearest {..} k x0 =
    let dsts = VS.toList $ RU.head $ toColumns $ pairwiseD2 knnX $ fromRows [VS.convert x0]
        res  = sortOn fst $ zip dsts $ V.toList knnY
    in take k $ snd <$> res

initKNN :: [V.Vector Double] -> V.Vector a -> KNearest a
initKNN xs = KNearest (fromColumns $ VS.convert <$> xs)
