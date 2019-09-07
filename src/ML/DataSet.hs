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
import           Data.Vector (Vector)
import qualified Data.Vector as V

class Summary a where
  summary :: a -> [Text]

-- select the subset of rows that are
type RowSelector = Int -> Bool

-- there is some duplication, columns are part of the feature, but we're just
-- exposing functions over our data so it's probably ok to just provide both.
data DataSet' = DataSet'
    { dsName'      :: T.Text
    , dsFeatures   :: [Feature Double]
    , dsColumns'   :: [Column Double]
    , dsNumRows'   :: Int
    , dsNumCols'   :: Int
    , colByName'   :: T.Text  -> Maybe (Column Double)
    , featByName   :: T.Text  -> Maybe (Feature Double)
    , featureSpace :: FeatureSpace
    }

-- meta data for a data set, defining the subset of the data that's
-- considered active in a given context (features available in a
-- dataset, features that where used to train a model,
-- result for variable selection algorithm, etc).
data FeatureSpace = FeatureSpace
    { findFeature :: Text -> Maybe FeatureSpec
    , knownFeats  :: [FeatureSpec]
    }

-- TODO: this is a subset of the information that's currently stored
-- as part of the Feature itself.
data FeatureSpec = FeatureSpec
    { featName          :: Text
    , column            :: Text
    , additionalColumns :: [Text]
    } deriving (Eq, Show)

data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Eq, Show)

data Categorical a = Categorical
    { className   :: Text
    , baseFeature :: Text
    , features    :: [Column a] } deriving (Eq, Show)

data Feature a = SingleCol (Column a)
               | MultiCol  (Categorical a) deriving (Eq, Show)

createFromFeatures :: T.Text -> [Feature Double] -> DataSet'
createFromFeatures name feats =
    let dsName'      = name
        dsFeatures   = feats
        dsColumns'   = concatMap featureColumns feats
        dsNumRows'   = fromMaybe 0 $ featureSize <$> listToMaybe feats
        dsNumCols'   = length dsColumns'
        columnIndex  = M.fromList $ zip (colName <$> dsColumns') dsColumns'
        featureIndex = M.fromList $ zip (featureName <$> feats) feats
        featByName f = M.lookup f featureIndex
        findMissingClass :: T.Text -> Maybe (Column Double)
        findMissingClass (splitFeatureName -> Just (feat, klass)) =
            featByName feat >>= \case
                SingleCol _                    -> Nothing
                MultiCol  c@Categorical { .. } ->
                    if baseFeature == klass
                       then return $ baselineColumn feat c
                       else return $ Column (feat <> "_" <> klass) $
                           V.replicate dsNumRows' 0.0
        findMissingClass _ = Nothing
        colByName' c = whenNothing (M.lookup c columnIndex) (findMissingClass c)
        featureSpace = createFeatureSpace $ createFeatureSpec <$> feats
    in DataSet' { .. }

-- TODO: Either Text ? notion of missing / spec mismatch?
extractDataColumns :: DataSet' -> FeatureSpace -> [Column Double]
extractDataColumns ds fs = concatMap (featureVectors' ds) $ knownFeats fs

filterDataColumn :: RowSelector -> Column a -> Column a
filterDataColumn rs (Column name cData) =
    Column name $ V.ifilter (\i _ -> rs i) cData

featureVectors' :: DataSet' -> FeatureSpec -> [Column Double]
featureVectors' ds FeatureSpec { .. } =
    if null additionalColumns
       then fromMaybe [] $ (:[]) <$> (colByName' ds column)
       else let colNames = ((featName <>) "_" <>) <$> additionalColumns
            in fromMaybe [] $ sequence $ colByName' ds <$> colNames

createFeatureSpec :: Feature a -> FeatureSpec
createFeatureSpec (MultiCol Categorical { .. }) = FeatureSpec
    { featName          = className
    , column            = baseFeature
    , additionalColumns = (getColName . colName) <$> features }
     where getColName :: Text -> Text
           getColName cn = fromMaybe "" $ snd <$> splitFeatureName cn
createFeatureSpec (SingleCol col) = FeatureSpec
    { featName          = colName col
    , column            = colName col
    , additionalColumns = [] }

createFeatureSpace :: [FeatureSpec] -> FeatureSpace
createFeatureSpace fs =
    let m = M.fromList $ zip (featName <$> fs) fs
    in  FeatureSpace (flip M.lookup m) (snd <$> M.toList m)

-- split a text value into a prefix before _, considered the feature name,
-- and the remaining text, considered the name of the class of the categorical
-- variable.
splitFeatureName :: T.Text -> Maybe (T.Text, T.Text)
splitFeatureName (T.split (=='_') -> (f:kl)) =
    if null kl
       then Nothing
       else Just (f, T.unwords kl)
splitFeatureName _ = Nothing

featureName :: Feature a -> Text
featureName (SingleCol Column { .. })      = colName
featureName (MultiCol  Categorical { .. }) = className

columnNames :: Feature a -> [Text]
columnNames (SingleCol Column { .. })      = [colName]
columnNames (MultiCol  Categorical { .. }) = colName <$> features

featureSize :: Feature a -> Int
featureSize (SingleCol col)                = colSize col
featureSize (MultiCol  Categorical { .. }) = maybe 0 colSize $ listToMaybe features

featureVectors :: Feature a -> [Vector a]
featureVectors (SingleCol Column { .. })     = [colData]
featureVectors (MultiCol Categorical { .. }) = colData <$> features

featureVector :: Feature a -> Vector a
featureVector (SingleCol Column { .. })     = colData
featureVector (MultiCol Categorical { .. }) =
    error $ "trying to extract a single feature from categorical column '"
        <> className <> "'"

featureColumn :: Feature a -> Column a
featureColumn (SingleCol c)     = c
featureColumn (MultiCol Categorical { .. }) =
    error $ "trying to extract a single feature from categorical column '"
        <> className <> "'"
baselineColumn :: Text -> Categorical Double -> Column Double
baselineColumn fName Categorical { .. } = Column
    { colName = fName <> "_" <> baseFeature
    , colData = v }
 where n      = maybe 0 colSize $ listToMaybe features
       vsum i = sum $ (V.! i) . colData <$> features
       v      = V.generate n (\i -> 1.0 - vsum i) :: Vector Double

featureColumns :: Feature a -> [Column a]
featureColumns (SingleCol col)               = [col]
featureColumns (MultiCol Categorical { .. }) = features

colSize :: Column a -> Int
colSize = V.length . colData

createFeature :: Text -> [Text] -> Feature Double
createFeature name []     = SingleCol $ Column name V.empty
createFeature name xs@(x:_) =
    let parseFirst = readEither x :: Either Text Double
    in case parseFirst of
        Right _ -> SingleCol $ createSingleCol name xs
        Left _  -> MultiCol  $ createCategorical name xs

replaceNAs :: Vector Double -> Vector Double
replaceNAs xs =
    let cleanVals = V.filter (not . isNaN) xs
        mean      = V.sum cleanVals / fromIntegral (V.length cleanVals)
    in V.map (\x -> if isNaN x then mean else x) xs

createSingleCol :: Text -> [Text] -> Column Double
createSingleCol colName colData =
    let fallback = sqrt $ -1
    in Column colName $ replaceNAs $ V.fromList $
            either (const fallback) identity . readEither <$> colData

createCategorical :: Text -> [Text] -> Categorical Double
createCategorical className colData =
    let klasses                    = -- debugShow ("klasses for " <> className) $
            sort . ordNub $ colData
        Just (baseFeature, others) = uncons klasses
        features                   = fmap createKlassVector others
        createKlassVector kl       = Column (className <> "_" <> kl) $ V.fromList $
            fmap (\d -> if d == kl then 1.0 else 0.0) colData
    in  Categorical { .. }

