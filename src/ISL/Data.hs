{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

{-
Very simple prototype of how to represent data
goals:
    - as simple as possible
    - regression only
    - everything's a double
-}

module ISL.Data where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.CSV (parseCSVFromFile, Record)

class Predictor a where
  predict :: a -> [Vector Double] -> Vector Double

data DataSet = DataSet
    { name        :: Text
    , columnNames :: Vector Text
    , columnData  :: [Vector Double]
    }

readCsvWithHeader :: FilePath -> IO DataSet
readCsvWithHeader f =
    parseCSVFromFile f >>= \case
        Right csv ->
            let columnNames = V.fromList $ T.pack <$> head csv
                columnData  = createColumns $ tail csv
                name        = T.pack f
            in return $ DataSet { .. }
        Left  err -> error $ show err

createColumns :: [Record] -> [Vector Double]
createColumns csv =
    let rows = length csv
        cols = length $ head csv
    in []

-- extractColumn :: (Read a, VS.Storable a) => Int -> [Record] -> VS.Vector a
-- extractColumn c rs = VS.fromList $ read .  (!! c) <$> tail (filter (/= [""]) rs)
