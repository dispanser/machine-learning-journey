{-# LANGUAGE OverloadedStrings #-}

module ML.Model.ValidationSuite where

import qualified ML.Dataset as DS
import           ML.Model.Validation
import           Control.Monad (forM_)
import           Data.List.NonEmpty(NonEmpty(..))
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec

fDouble, fNAs, fCat :: DS.Feature
fDouble       = DS.createFeature "col1" ("1.0" :| ["2.0", "3.0"])
fNAs          = DS.createFeature "col2" ("1.0" :| ["NA", "3.0"])
fCat          = DS.createFeature "col3" ("red" :| ["yellow", "blue"])

spec_NegateRS :: Spec
spec_NegateRS =
    describe "negating row selector" $ do
        let rs    = even :: DS.RowSelector
            negRS = negateRowSelector rs
        it "should be true iff the underlying is false" $
            forM_ [0..10] (\i -> rs i `shouldBe` (not $ negRS i))

