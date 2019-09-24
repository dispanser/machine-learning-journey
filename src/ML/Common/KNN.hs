{-# LANGUAGE RecordWildCards #-}

module ML.Common.KNN where

import qualified Relude.Unsafe as RU
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra (Matrix, R, fromColumns, fromRows, pairwiseD2, toColumns)

-- the type of the response values is kept generic, to be able to support
-- quantitative values for regression and qualitative values for classification.
data KNearest a = KNearest
    { knnX :: Matrix R
    , knnY :: VS.Vector a
    }

-- little helper to find the k nearest neighbors for a single observation
findNearest :: VS.Storable a => KNearest a -> Int -> VS.Vector R -> [a]
findNearest KNearest {..} k x0 =
    let dsts = VS.toList $ RU.head $ toColumns $ pairwiseD2 knnX $ fromRows [x0]
        res  = sortOn fst $ zip dsts $ VS.toList knnY
    in take k $ snd <$> res

initKNN :: VS.Storable a => [V.Vector Double] -> V.Vector a -> KNearest a
initKNN xs y = KNearest (fromColumns $ VS.convert <$> xs) $ VS.convert y
