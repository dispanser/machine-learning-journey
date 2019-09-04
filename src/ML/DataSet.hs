{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-
Very simple prototype of how to represent data

This is far away from being usable:
- we assume everything's a double
- no support for categorical features
- no support for N/A
-}

module ML.DataSet where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           ISL.Model (Feature(..), Column(..), Categorical(..),
                        featureColumns, featureSize, featureName, baselineColumn)

-- there is some duplication, columns are part of the feature, but we're just
-- exposing functions over our data so it's probably ok to just provide both.
data DataSet' = DataSet'
    { dsName'    :: T.Text
    , dsFeatures :: [Feature Double]
    , dsColumns' :: [Column Double]
    , dsNumRows' :: Int
    , dsNumCols' :: Int
    , colByName' :: T.Text -> Maybe (Column Double)
    , featByName :: T.Text -> Maybe (Feature Double)
    }

createFromFeatures :: T.Text -> [Feature Double] -> DataSet'
createFromFeatures name feats =
    let dsFeatures   = feats
        dsName'      = name
        dsColumns'   = concatMap featureColumns feats
        dsNumRows'   = fromMaybe 0 $ featureSize <$> listToMaybe feats
        dsNumCols'   = length dsColumns'
        columnIndex  = M.fromList $ zip (colName <$> dsColumns') dsColumns'
        featureIndex = M.fromList $ zip (featureName <$> dsFeatures) dsFeatures
        featByName f = M.lookup f featureIndex
        findMissingClass :: T.Text -> Maybe (Column Double)
        findMissingClass (splitFeatureName -> Just (feat, klass)) =
            featByName feat >>= \case
                SingleCol _                    -> Nothing
                MultiCol  c@Categorical { .. } ->
                    if baseFeature == klass
                       then return $ baselineColumn feat c
                       else return $ Column (feat <> "_" <> klass) $ V.replicate dsNumRows' 0.0
        findMissingClass _ = Nothing
        colByName' c = whenNothing (M.lookup c columnIndex) (findMissingClass c)
    in DataSet' { .. }

-- split a text value into a prefix before _, considered the feature name,
-- and the remaining text, considered the name of the class of the categorical
-- variable.
splitFeatureName :: T.Text -> Maybe (T.Text, T.Text)
splitFeatureName (T.split (=='_') -> (f:kl)) =
    if null kl
       then Nothing
       else Just (f, T.unwords kl)
splitFeatureName _ = Nothing
