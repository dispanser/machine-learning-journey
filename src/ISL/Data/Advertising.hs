{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}

module ISL.Data.Advertising where

import           Text.CSV (parseCSVFromFile, Record)
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable (Vector)

data Advertising = Advertising
    { salesCol :: Vector Double
    , tvCol    :: Vector Double
    , radioCol :: Vector Double
    , newspCol :: Vector Double }

readAdvertisingData :: IO Advertising
readAdvertisingData =
    parseCSVFromFile "data/Advertising.csv" >>= \case
        Right adData -> do
            let salesCol = extractColumn 4 adData :: Vector Double
                tvCol    = extractColumn 1 adData :: Vector Double
                radioCol = extractColumn 2 adData :: Vector Double
                newspCol = extractColumn 3 adData :: Vector Double
            return $ Advertising { .. }
        Left err -> error $ show err

extractColumn :: (Read a, V.Storable a) => Int -> [Record] -> V.Vector a
extractColumn c rs = V.fromList $ read .  (!! c) <$> tail (filter (/= [""]) rs)

