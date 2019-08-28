{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

{-
Very simple prototype of how to represent data
goals:
    - as simple as possible
    - regression only
    - everything's a double

ideas:
    - extractFeatureVector should produce an Either, indicating what went wrong
      (or some other kind of validation approach)
    - create a formula language similar to R, where "sales ~ tv + radio + ..."
      - alternatively, more of a DSL, e.g.: "sales" onto "tv" plus "radio" ...
    - combine this formula language into jjj

-}

module ISL.DataSet where

import qualified Data.Map.Strict as M
import           Data.Vector (Vector)
import           Data.Text (Text)

class Predictor a where
  predict :: a -> [Column Double] -> Column Double

class Summary a where
  summary :: a -> Text

class ModelFit a where
  fit :: ModelInput -> a

-- represents the input data, i.e. the housing dataset in its raw form
data DataSet = DataSet
    { dsName          :: Text
    , dsColumnIndices :: M.Map Text Int
    , dsColumnData    :: [Vector Double]
    } deriving        (Show)

-- TODO: better name, please
-- represents the input to a fit procedure: a data set and the
-- names of the features, the name of the response etc.
-- at this stage, the inputs are verified
data ModelInput = ModelInput
    { miName     :: !Text
    , miFeatures :: ![Column Double]
    , miResponse :: Column Double }

data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Show, Eq, Ord)

extractFeatureVector :: Text -> DataSet -> Maybe (Column Double)
extractFeatureVector colName DataSet { .. } =
    Column colName . (dsColumnData !!) <$> M.lookup colName dsColumnIndices


extractFeatureVectors :: [Text] -> DataSet -> Maybe [Column Double]
extractFeatureVectors colNames ds = traverse (flip extractFeatureVector ds) colNames

extractModelInput :: Text -> [Text] -> DataSet -> Maybe ModelInput
extractModelInput responseName featureNames ds@DataSet { .. }  = do
    featureCols <- extractFeatureVectors featureNames ds
    responseCol <- extractFeatureVector  responseName ds
    return ModelInput
        { miName     = dsName
        , miFeatures = featureCols
        , miResponse = responseCol }

