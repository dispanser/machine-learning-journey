{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ML.Dataset.CSV where

import qualified Relude.Unsafe as RU
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           ML.Dataset (Dataset(..), createFromFeatures)
import           ML.Dataset (Column(..) , featureName , createFeature)
import           Text.CSV (parseCSVFromFile, Record, printCSV)

-- create a dataset, the new way
createDataset :: FilePath -> IO Dataset
createDataset = createDataset' (const True)

-- create a dataset, using a predicate on the column name to
-- reduce the number of columns

type Pred a = a -> Bool

createDataset' :: Pred Text -> FilePath -> IO Dataset
createDataset' p f =
    parseCSVFromFile f >>= \case
        Right (header:body) ->
            let bodyColumns = (toText <$>) <$> transpose (filter (/= [""]) body)
                name        = toText f
                features = filter (p . featureName) $
                    zipWith createFeature (toText <$> header) bodyColumns
            in return $ createFromFeatures name features
        Right [] -> error $ "no data: empty csv"
        Left  err -> error $ show err

-- TODO: use mutable, pre-allocated vectors for performance
createColumns :: Int -> [Record] -> [Vector T.Text]
createColumns cols csv = flip extractColumn csv <$> [0 .. cols - 1]

extractColumn :: Int -> [Record] -> Vector T.Text
extractColumn c rs =
    let wtf = T.pack . (RU.!! c) <$> filter (/= [""]) rs
    in V.fromList wtf

writeCsv :: FilePath -> [(Column Double, Double -> String)] -> IO ()
writeCsv fp columns = writeFile fp $ printCSV (header:body)
 where header             = T.unpack . colName . fst <$> columns
       (cols, formatters) = unzip columns
       numRows            = fromMaybe 0 $ VU.length . colData . fst <$> listToMaybe columns
       -- createRow :: Int -> [String]
       createRow i = zipWith ($) formatters ((VU.! i) . colData <$> cols)
       body        = createRow <$> [0..numRows-1]

