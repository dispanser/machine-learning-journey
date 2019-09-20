{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Column.Internal
    ( RowSelector
    , ColumnTransformer
    , Column(..)
    , filterDataColumn
    , columnVariance
    , mkColumn
    ) where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import           ML.Data.Summary

type RowSelector       = Int           -> Bool
type ColumnTransformer = Vector Double -> Vector Double

data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Eq, Show)

-- type TransformRule  = VB.Vector Text           -> [Column Double]

-- handles the ingestion of a single feature, transforming the text values
-- into a numerical representation.
-- type FeatureParser  = VB.Vector Text -> ([Column Double], TransformRule)

mkColumn :: Text -> Vector a -> Column a
mkColumn = Column

filterDataColumn :: V.Unbox a => RowSelector -> Vector a -> Vector a
filterDataColumn rs xs =V.ifilter (\i _ -> rs i) xs

columnVariance :: Vector Double -> Double
columnVariance = S.varianceUnbiased
