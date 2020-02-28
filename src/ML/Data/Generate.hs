{-# LANGUAGE FlexibleContexts #-}

module ML.Data.Generate where

import           System.Random.MWC.Probability (Gen, standardNormal, samples)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad (replicateM)
import           Numeric.LinearAlgebra (Matrix, R, fromColumns)

standardNormalV :: (PrimMonad m) => Gen (PrimState m) -> Int -> m (VS.Vector Double)
standardNormalV s n = VG.fromList <$> samples n standardNormal s

standardNormalM :: (PrimMonad m) => Gen (PrimState m) -> Int -> Int -> m (Matrix R)
standardNormalM s m n = do
    columns <- replicateM n $ VG.convert <$> standardNormalV s m
    return $ fromColumns columns
