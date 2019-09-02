{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
Very simple prototype of how to represent data

This is far away from being usable:
- we assume everything's a double
- no support for categorical features
- no support for N/A
-}

module ISL.DataSet where

import           Data.Text (Text)

class Summary a where
  summary :: a -> [Text]

-- represents the input data, i.e. the housing dataset in its raw form
class DataSet' a where
  dsNames'   :: a -> [Text]
  dsDims'    :: a -> (Int, Int)
  dsCols'    :: a -> Int
  dsRows'    :: a -> Int
  colByName' :: a -> Text -> Maybe [Text]


data DataSet = DataSet
    { dsName    :: Text
    , dsColumns :: [Text]
    , dsNumCols :: Int
    , dsNumRows :: Int
    , colByName :: Text -> Maybe [Text] }

-- instance Summary DataSet where
--   summary ds = [ "DataSet:    " <> dsName ds
--                , "dimensions: " <> show (numRows ds) <> "x" <> show (numCols ds)
--                , "columns:    " <> (T.intercalate ", " $ names ds)]

