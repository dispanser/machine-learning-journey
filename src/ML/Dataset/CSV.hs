{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ML.Dataset.CSV where

import           Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import           ML.Dataset (RawData(..))
import           ML.Data.Column.Internal (Column(..))
import           Text.CSV (parseCSVFromFile, printCSV)

-- create a dataset, the new way
readRawData :: FilePath -> IO RawData
readRawData = readRawData' (const True)

-- read raw data from input
readRawData' :: Pred Text -> FilePath -> IO RawData
readRawData' p f = do
    csv <- parseCSVFromFile f
    let cleaned = filter acceptRecord <$> csv
    case cleaned of
        Right (header:body) ->
            let bodyColumns = (toText <$>) <$> transpose body
                fullHeaders = toText <$> header
                headers     = filter p $ fullHeaders
                features    = filter (p . fst) $ zip fullHeaders bodyColumns
                lookupCol n = do
                    res <- maybeToRight ("unknown feature '" <> show n <>
                        "', available: " <> show headers) $ lookup n features
                    case res of
                      []      -> Left "empty data"
                      (x:xs)  -> Right (x :| xs)
            in return RawData
                { names      = headers
                , dataColumn = lookupCol
                }
        Right []  -> error $ "no data: empty csv"
        Left  err -> error $ show err

acceptRecord :: [String] -> Bool
acceptRecord [""]  = False
acceptRecord (x:_) = not $ "#" `isPrefixOf` x
acceptRecord _     = True

writeCsv :: FilePath -> [(Column Double, Double -> String)] -> IO ()
writeCsv fp columns = writeFile fp $ printCSV (header:body)
 where header             = T.unpack . colName . fst <$> columns
       (cols, formatters) = unzip columns
       numRows            = fromMaybe 0 $ VU.length . colData . fst <$> listToMaybe columns
       -- createRow :: Int -> [String]
       createRow i = zipWith ($) formatters ((VU.! i) . colData <$> cols)
       body        = createRow <$> [0..numRows-1]

