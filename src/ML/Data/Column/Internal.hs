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

import           Data.Text (Text)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S

type RowSelector       = Int           -> Bool
type ColumnTransformer = Vector Double -> Vector Double

data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Eq, Show)

mkColumn :: Text -> Vector a -> Column a
mkColumn = Column

filterDataColumn :: V.Unbox a => RowSelector -> Vector a -> Vector a
filterDataColumn rs xs =V.ifilter (\i _ -> rs i) xs

columnVariance :: Vector Double -> Double
columnVariance = S.varianceUnbiased
