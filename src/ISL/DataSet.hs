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
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.CSV (parseCSVFromFile, Record)

class Predictor a where
  predict :: a -> [Vector Double] -> Vector Double

class Summary a where
  summary :: a -> String

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
    , colData :: Vector a }

readCsvWithHeader :: FilePath -> IO DataSet
readCsvWithHeader f =
    parseCSVFromFile f >>= \case
        Right csv ->
            let dsColumnIndices = M.fromList $ zip (T.pack <$> head csv) [0..]
                dsColumnData    = createColumns $ tail csv
                dsName          = T.pack f
            in return $ DataSet { .. }
        Left  err -> error $ show err

extractFeatureVector :: Text -> DataSet -> Maybe (Vector Double)
extractFeatureVector colName DataSet { .. } =
    (dsColumnData !!) <$> M.lookup colName dsColumnIndices

extractFeatureVectors :: [Text] -> DataSet -> Maybe [Vector Double]
extractFeatureVectors colNames ds = traverse (flip extractFeatureVector ds) colNames

extractModelInput :: [Text] -> DataSet -> Maybe DataSet
extractModelInput colNames ds@DataSet { .. }  = do
    cn <- extractFeatureVectors colNames ds
    return ds { dsColumnIndices = M.fromList $ zip colNames [0..]
              , dsColumnData    = cn }

-- TODO: use mutable, pre-allocated vectors for performance
createColumns :: [Record] -> [Vector Double]
createColumns csv =
    let cols = length $ head csv
    in  flip extractColumn csv <$> [0 .. cols - 1]

extractColumn :: (Read a) => Int -> [Record] -> Vector a
extractColumn c rs = V.fromList $ read .  (!! c) <$> filter (/= [""]) rs
