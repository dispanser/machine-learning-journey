module Base where

import ML.Dataset
import Data.Vector.Unboxed

contFeat :: Text -> Vector Double -> Feature
contFeat name' xs = Feature (Continuous name' 0 1) [xs]

catFeat :: Text -> NonEmpty Text-> Feature
catFeat = createCategorical
