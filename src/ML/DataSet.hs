{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
Very simple prototype of how to represent data

This is far away from being usable:
- we assume everything's a double
- no support for categorical features
- no support for N/A
-}

module ML.DataSet where

import           Data.Text (Text)
import           ISL.Model (Feature, Column, featureColumns)

-- there is some duplication, columns are part of the feature, but we're just
-- exposing functions over our data so it's probably ok to just provide both.
data DataSet' = DataSet'
    { dsName'    :: Text
    , dsFeatures :: [Feature Double]
    , dsColumns' :: [Column Double]
    , dsNumRows' :: Int
    , dsNumCols' :: Int
    , colByName' :: Text -> Maybe (Column Double)
    , featByName :: Text -> Maybe (Feature Double)
    }

createFromFeatures :: Text -> [Feature Double] -> DataSet'
createFromFeatures name features =
    let dsFeatures = features
        dsName'    = name
        dsColumns' = concatMap featureColumns features
        dsNumRows' = undefined
        dsNumCols' = length dsColumns'
        colByName' = undefined
        featByName = undefined
    in DataSet' { .. }

