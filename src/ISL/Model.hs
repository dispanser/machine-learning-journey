{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE PartialTypeSignatures#-}
{-# LANGUAGE ScopedTypeVariables #-}

module ISL.Model where

import qualified Relude.Unsafe as RU
import qualified ML.DataSet as DS
import           ML.DataSet (DataSet'(..), Feature(..), Column(..)
                            , FeatureSpace(..) , FeatureSpec(..))
import           Data.Text (Text)

-- initial stub type for prediction result: the column vector of response
type Prediction = Feature Double

class Predictor a where
  predict  :: a -> DataSet'                   -> Prediction
  predict' :: a -> DataSet' -> DS.RowSelector -> Prediction

class ModelFit a where
  fit  :: DataSet' -> ModelSpec                   -> a
  fit' :: DataSet' -> ModelSpec -> DS.RowSelector -> a

data ModelSpec = ModelSpec
    { features' :: FeatureSpace
    , response  :: FeatureSpec }

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

extractResponseVector :: DataSet' -> ModelSpec -> Column Double
extractResponseVector ds ms = RU.head $ DS.featureVectors' ds (response ms)

