{-# LANGUAGE FlexibleContexts #-}

module ML.Classification where

import qualified Data.Vector.Generic as GV

class Classifier a where
  classify :: GV.Vector v Double => a -> Int -> v Double -> [(Double, b)]

