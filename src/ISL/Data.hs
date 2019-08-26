{-# LANGUAGE OverloadedStrings #-}

{-
Very simple prototype of how to represent data
goals:
    - as simple as possible
    - regression only
    - everything's a double
-}

module ISL.Data where

import           Data.Vector.Storable (Vector)

class Predictor a where
  predict :: a -> [Vector Double] -> Vector Double
