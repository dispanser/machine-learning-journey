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

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

class Summary a where
  summary :: a -> [Text]

-- represents the input data, i.e. the housing dataset in its raw form
data DataSet = DataSet
    { dsName          :: Text
    , dsColumnIndices :: M.Map Text (Vector Text)
    } deriving        (Show)

instance Summary DataSet where
  summary ds = [ "DataSet:    " <> dsName ds
               , "dimensions: " <> show (numRows ds) <> "x" <> show (numCols ds)
               , "columns:    " <> (T.intercalate ", " $ names ds)]

names :: DataSet -> [Text]
names = M.keys . dsColumnIndices

dimenions :: DataSet -> (Int, Int)
dimenions ds = (numRows ds, numCols ds)

numRows :: DataSet -> Int
numRows ds =
    fromMaybe 0 $ V.length . fst <$> (uncons $ snd <$> (M.toList $ dsColumnIndices ds))

numCols :: DataSet -> Int
numCols ds = M.size $ dsColumnIndices ds
