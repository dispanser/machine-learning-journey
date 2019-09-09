{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE PartialTypeSignatures#-}
{-# LANGUAGE ScopedTypeVariables #-}

module ML.Model where

import qualified Relude.Unsafe as RU
import qualified ML.Dataset as DS
import           ML.Dataset (Dataset(..), Feature(..), Column(..)
                            , FeatureSpace(..) , FeatureSpec(..))
import           Data.Text (Text)

-- initial stub type for prediction result: the column vector of response
type Prediction = Feature Double

class Predictor a where
  predict  :: a -> Dataset                   -> Prediction
  predict' :: a -> Dataset -> DS.RowSelector -> Prediction

class ModelFit a where
  fit  :: Dataset -> ModelSpec                   -> a
  fit' :: Dataset -> ModelSpec -> DS.RowSelector -> a

data ModelSpec = ModelSpec
    { features' :: FeatureSpace
    , response  :: FeatureSpec } deriving (Show)

buildModelSpec :: FeatureSpace -> Text -> [Text] -> Either Text ModelSpec
buildModelSpec FeatureSpace {..} responseName featureNames = do
    response <- maybe (Left $ "response named '" <> responseName <> "' not found")
        Right $ findFeature responseName
    features <- forM featureNames (\fn ->
        maybe (Left $ "feature named '" <> fn <> "' not found")
                   Right $ findFeature fn)
    return ModelSpec
        { features' = DS.createFeatureSpace features
        , response  = response }

extractResponseVector :: Dataset -> ModelSpec -> Column Double
extractResponseVector ds ms = RU.head $ DS.featureVectors' ds (response ms)

