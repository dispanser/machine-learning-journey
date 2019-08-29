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
import           Data.Vector (Vector)

-- represents the input data, i.e. the housing dataset in its raw form
data DataSet = DataSet
    { dsName          :: Text
    , dsColumnIndices :: M.Map Text Int
    , dsColumnData    :: [Vector Double]
    } deriving        (Show)

