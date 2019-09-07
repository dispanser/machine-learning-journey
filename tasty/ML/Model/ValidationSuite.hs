{-# LANGUAGE OverloadedStrings #-}

module ML.Model.ValidationSuite where

import qualified ML.DataSet as DS
import           ISL.Model.Validation
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

fDouble, fNAs, fCat :: DS.Feature Double
fDouble       = DS.createFeature "col1" ["1.0", "2.0", "3.0"]
fNAs          = DS.createFeature "col2" ["1.0", "NA", "3.0"]
fCat          = DS.createFeature "col3" ["red", "yellow", "blue"]

spec_NegateRS :: Spec
spec_NegateRS = do
    describe "negating row selector" $ do
        let rs    = even :: DS.RowSelector
            negRS = negateRowSelector rs
        it "should be true iff the underlying is false" $ do
            forM_ [0..10] (\i -> rs i `shouldBe` (not $ negRS i))

