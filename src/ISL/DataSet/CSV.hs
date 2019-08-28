{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ISL.DataSet.CSV where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           ISL.DataSet (DataSet(..), Column(..))
import           Text.CSV (parseCSVFromFile, Record, printCSV)

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

writeCsv :: FilePath -> [Column Double] -> IO ()
writeCsv fp columns = writeFile fp $ printCSV (header:body)
 where header      = T.unpack . colName <$> columns
       rows        = V.length $ colData $ head columns
       createRow i = show . (V.! i) . colData <$> columns
       body        = createRow <$> [0..rows-1]

