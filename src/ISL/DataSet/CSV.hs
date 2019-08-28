{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ISL.DataSet.CSV where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           ISL.DataSet (DataSet(..))
import           Text.CSV (parseCSVFromFile, Record)

readCsvWithHeader :: FilePath -> IO DataSet
readCsvWithHeader f =
    parseCSVFromFile f >>= \case
        Right csv ->
            let dsColumnIndices = M.fromList $ zip (T.pack <$> head csv) [0..]
                dsColumnData    = createColumns $ tail csv
                dsName          = T.pack f
            in return $ DataSet { .. }
        Left  err -> error $ show err

-- TODO: use mutable, pre-allocated vectors for performance
createColumns :: [Record] -> [Vector Double]
createColumns csv =
    let cols = length $ head csv
    in  flip extractColumn csv <$> [0 .. cols - 1]

extractColumn :: (Read a) => Int -> [Record] -> Vector a
extractColumn c rs = V.fromList $ read .  (!! c) <$> filter (/= [""]) rs
