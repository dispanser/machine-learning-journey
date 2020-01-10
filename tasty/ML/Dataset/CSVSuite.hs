{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ML.Dataset.CSVSuite where


import           ML.Dataset.CSV (readRawData, readRawData')
import           ML.Dataset (Dataset(..), featureName, RawData(..), parseDataset
                            , parseFullDataset)
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

spec_parseDataset :: Spec
spec_parseDataset = do
    it "parseDataset" $ do
        parseDataset ["cat1", "cat2"] <$>
            readRawData "data/categorical.csv" >>= \case
                Left err -> fail err
                Right Dataset { .. } -> do
                    featureName <$> dsFeatures `shouldBe` ["cat1", "cat2"]
                    let Just col1 = colByName' "cat1_C"
                    col1 `shouldBe` [1, 0, 1, 0, 0]
                    let Just col2 = colByName' "cat2_Z"
                    col2 `shouldBe` [0, 0, 0, 1, 0]


spec_readRawData :: Spec
spec_readRawData = do
    it "CSV.readRawData should read all columns" $ do
        RawData { .. } <- readRawData "data/categorical.csv"
        dataColumn "cat1" `shouldBe` Right ["C", "B", "C", "A", "B"]
        dataColumn "cat2" `shouldBe` Right ["X", "X", "X", "Z", "Y"]
        dataColumn "cat3" `shouldBe` Left "unknown feature '\"cat3\"', available: [\"cat1\",\"cat2\"]"
    it "CSV.readRawData' should only read requested columns" $ do
        RawData { .. } <- readRawData' (=="cat1")  "data/categorical.csv"
        names `shouldBe` ["cat1"]
        dataColumn "cat1" `shouldBe` Right ["C", "B", "C", "A", "B"]
        dataColumn "cat2" `shouldBe` Left "unknown feature '\"cat2\"', available: [\"cat1\"]"
    it "should ignore lines with comments" $ do
        RawData { .. } <- readRawData "data/withcomments.csv"
        dataColumn "cat1" `shouldBe` Right ["C", "B", "C", "A", "B"]
        dataColumn "cat2" `shouldBe` Right ["X", "X", "X", "Z", "Y"]
        dataColumn "cat3" `shouldBe` Left "unknown feature '\"cat3\"', available: [\"cat1\",\"cat2\"]"


