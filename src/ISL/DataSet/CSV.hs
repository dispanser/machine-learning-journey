{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ISL.DataSet.CSV where

import qualified Relude.Unsafe as RU
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           ISL.DataSet (DataSet(..))
import           ISL.Model (Column(..))
import           Text.CSV (parseCSVFromFile, Record, printCSV)

readCsvWithHeader :: FilePath -> IO DataSet
readCsvWithHeader f =
    parseCSVFromFile f >>= \case
        Right csv ->
            let (header, body)  = fromMaybe ([], []) $ uncons csv
                cols            = length header
                dsColumnData    = createColumns cols body
                dsColumnIndices = M.fromList $ zip (T.pack <$> header) dsColumnData
                dsName          = T.pack f
            in return $ DataSet { .. }
        Left  err -> error $ show err

-- TODO: use mutable, pre-allocated vectors for performance
createColumns :: Int -> [Record] -> [Vector Double]
createColumns cols csv = flip extractColumn csv <$> [0 .. cols - 1]

extractColumn :: Int -> [Record] -> Vector Double
extractColumn c rs =
    let wtf = readEither . (RU.!! c) <$> filter (/= [""]) rs
    in V.fromList $ either (const $ sqrt (-1)) identity <$> wtf

writeCsv :: FilePath -> [Column Double] -> IO ()
writeCsv fp columns = writeFile fp $ printCSV (header:body)
 where header      = T.unpack . colName <$> columns
       rows        = V.length $ colData $ RU.head columns
       createRow i = show . (V.! i) . colData <$> columns
       body        = createRow <$> [0..rows-1]
