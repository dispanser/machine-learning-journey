{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures#-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE ScopedTypeVariables #-}

module ML.Model where

import qualified Relude.Unsafe as RU
import qualified ML.Dataset as DS
import           ML.Dataset (Dataset(..), Feature, FeatureSpace(..)
                            , Column(..), FeatureSpec)
import           Data.Text (Text)

-- initial stub type for prediction result: the column vector of response
type Prediction = Feature Double

type FitF  a = [Column Double] -> Column Double -> a

data ModelInit a = ModelInit
    { fitF      :: FitF a
    , modelSpec :: ModelSpec }

class Predictor a => Model a where
  features :: a -> FeatureSpace

-- something that can make predictions. It also knows the feature space,
-- because that helps writing generic functions that directly work on dataset
class Predictor a where
  predict :: a -> [Column Double] -> Prediction

data ModelSpec = ModelSpec
    { features' :: FeatureSpace
    , response  :: FeatureSpec } deriving (Show)

fitDataset :: ModelInit a -> Dataset -> a
fitDataset mi = fit' mi identity

fitSubset :: ModelInit a -> DS.RowSelector -> Dataset -> a
fitSubset mi rs = fit' mi $ DS.filterDataColumn rs

fit' :: ModelInit a -> DS.ColumnTransformer -> Dataset -> a
fit' ModelInit { .. } ct ds = fitF featureCols responseCol
 where responseCol = ct $ RU.head $ DS.featureVectors' ds $ response modelSpec
       featureCols = ct <$> (DS.extractDataColumns ds $ features' modelSpec)

predictDataset :: Model a => a -> Dataset -> Prediction
predictDataset pr = predict' pr identity

predict' :: Model a => a -> DS.ColumnTransformer -> Dataset -> Prediction
predict' pr ct ds = predict pr featureCols
 where featureCols = ct <$> (DS.extractDataColumns ds $ features pr)

predictSubset :: Model a => a -> DS.RowSelector -> Dataset -> Prediction
predictSubset pr rs = predict' pr $ DS.filterDataColumn rs

buildModelSpec :: FeatureSpace -> Text -> [Text] -> Either Text ModelSpec
buildModelSpec FeatureSpace {..} responseName featureNames = do
    response <- maybe (Left $ "response named '" <> responseName <> "' not found")
        Right $ findFeature responseName
    fs <- forM featureNames (\fn ->
        maybe (Left $ "feature named '" <> fn <> "' not found")
                   Right $ findFeature fn)
    return ModelSpec
        { features' = DS.createFeatureSpace fs
        , response  = response }

extractResponseVector :: Dataset -> ModelSpec -> Column Double
extractResponseVector ds ms = RU.head $ DS.featureVectors' ds (response ms)

