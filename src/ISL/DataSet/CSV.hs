{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ISL.DataSet.CSV where

import qualified Relude.Unsafe as RU
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           ISL.DataSet (DataSet(..))
import           ISL.Model (Feature(..), Column(..), featureName, featureSize)
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
createColumns :: Int -> [Record] -> [Vector T.Text]
createColumns cols csv = flip extractColumn csv <$> [0 .. cols - 1]

extractColumn :: Int -> [Record] -> Vector T.Text
extractColumn c rs =
    let wtf = T.pack . (RU.!! c) <$> filter (/= [""]) rs
    in V.fromList wtf

writeCsv :: FilePath -> [Feature Double] -> IO ()
writeCsv fp features = writeFile fp $ printCSV (header:body)
 where header      = T.unpack . featureName <$> features
       rows        = fromMaybe 0 $ featureSize <$> listToMaybe features
       createRow i = cellText i <$> features
       body        = createRow <$> [0..rows-1]

cellText :: Show a => Int -> Feature a -> String
cellText i (SingleCol Column { .. } )     = show $ colData V.! i
-- cellText i (MultiCol  Categorical { .. }) = colSize $ RU.head features

