{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ML.Dataset
  ( RowSelector
  , ColumnTransformer
  , module ML.Dataset -- TODO: replace module export with concrete stuff
  , C.filterDataColumn
  , C.columnVariance
  , FeatureType(..)
  , Feature(..)
  , FeatureSpace(..)
  , Metadata(..)
  , F.createFeature
  , F.featureName
  , F.createCategorical
  ) where

import           GHC.Show (Show(..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           ML.Data.Summary
import qualified ML.Data.Column.Internal as C
import           ML.Data.Column.Internal (RowSelector , ColumnTransformer)
import qualified ML.Data.Feature.Internal as F
import           ML.Data.Feature.Internal ( FeatureType(..),  Feature(..)
                                          , FeatureSpace(..), Metadata(..))
type ParseError = String

data RawData = RawData
    { names      :: [Text]
    , dataColumn :: Text -> Either ParseError (NonEmpty Text)
    }

-- there is some duplication, columns are part of the feature, but we're just
-- exposing functions over our data so it's probably ok to just provide both.
data Dataset = Dataset
    { dsName'      :: T.Text
    , dsFeatures   :: [Feature]
    , dsNumRows'   :: Int
    , dsNumCols'   :: Int
    , colByName'   :: T.Text  -> Maybe (Vector Double)
    , featByName   :: T.Text  -> Maybe Feature
    , featureSpace :: FeatureSpace
    }

instance Show Dataset where
  show = toString . unlines . summary

instance Summary Dataset where
  summary ds = concatMap summary $ dsFeatures ds

parseDataset :: [FeatureType] -> RawData -> Either ParseError Dataset
parseDataset fs RawData {..} =
    let feature ft = F.createFeature ft <$> dataColumn (name ft)
        features   = traverse feature fs
    in createFromFeatures "unknown dataset" <$> features

parseFullDataset :: RawData -> Either ParseError Dataset
parseFullDataset = parseScaledDataset F.noScaling

parseScaledDataset :: F.ScaleStrategy -> RawData -> Either ParseError Dataset
parseScaledDataset ss rd =
    let features = Auto ss <$> names rd
    in parseDataset features rd

createFromFeatures :: T.Text -> [Feature] -> Dataset
createFromFeatures name feats =
    let dsName'      = name
        dsFeatures   = sortOn F.featureName feats
        featureSpace = createFeatureSpace $ metadata <$> dsFeatures
        columnsInSet = concatMap columns dsFeatures
        dsNumRows'   = fromMaybe 0 (V.length <$> (listToMaybe columnsInSet))
        dsNumCols'   = length columnsInSet
        columnIndex  = M.fromList $ zip (columnNames featureSpace) columnsInSet
        featureIndex = M.fromList $ zip (F.featureName <$> dsFeatures) dsFeatures
        featByName f = M.lookup f featureIndex
        findMissingClass :: T.Text -> Maybe (Vector Double)
        findMissingClass (splitFeatureName -> Just (feat, klass)) =
            featByName feat >>= \case
                Feature (Continuous _ _) _ -> Nothing
                (Feature (Categorical _ baseFeature _) vss) ->
                    if baseFeature == klass
                       then return $ F.baselineColumn vss
                       else return $ V.replicate dsNumRows' 0.0
        findMissingClass _ = Nothing
        colByName' c = whenNothing (M.lookup c columnIndex) (findMissingClass c)
    in Dataset { .. }

extractDataColumns :: Dataset -> FeatureSpace -> [Vector Double]
extractDataColumns ds FeatureSpace { .. } =
    concatMap (featureVectors' ds) knownFeats

featureVectors' :: Dataset -> Metadata -> [Vector Double]
featureVectors' ds Categorical {..} =
    let colNames = ((featName' <>) "_" <>) <$> otherLabels
    in fromMaybe [] $ sequence $ colByName' ds <$> colNames
featureVectors' ds (featName' -> name) = maybe [] columns $ featByName ds name


       -- then fromMaybe [] $ (:[]) <$> (colByName' ds column)
createFeatureSpace :: [Metadata] -> FeatureSpace
createFeatureSpace mds =
    let m = M.fromList $ zip (featName' <$> mds) mds
    in  FeatureSpace (flip M.lookup m) (snd <$> M.toList m) []

-- split a text value into a prefix before _, considered the feature name,
-- and the remaining text, considered the name of the class of the categorical
-- variable.
splitFeatureName :: T.Text -> Maybe (T.Text, T.Text)
splitFeatureName (T.split (=='_') -> (f:kl)) =
    if null kl
       then Nothing
       else Just (f, T.unwords kl)
splitFeatureName _ = Nothing

rowSelectorFromList :: [Bool] -> RowSelector
rowSelectorFromList xs =
    let v = V.fromList xs
    in (v V.!)

-- names of the columns that we would produce when extracting the data from a
-- dataset, using the given feature space as reference. This does not include
-- the base labels of categorical variables, because they don't appear in the
-- model input (one-hot encoding)
columnNames :: FeatureSpace -> [Text]
columnNames = concatMap featColNames . knownFeats

featColNames :: Metadata -> [Text]
featColNames Continuous {..}  = [featName']
featColNames Categorical {..} = ((featName' <> "_") <>) <$> otherLabels
