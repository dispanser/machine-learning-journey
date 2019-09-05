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

