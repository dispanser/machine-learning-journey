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
  ) where

import           GHC.Show (Show(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           ML.Data.Summary
import qualified ML.Data.Column.Internal as C
import           ML.Data.Column.Internal ( RowSelector , ColumnTransformer
                                         , summarizeVector)
type ParseError = String

data FeatureType =
    Auto { name :: Text }
      | Cat { name :: Text }
      | Cont { name :: Text }

-- instantiate a string into a feature type using auto-detection mechanism
instance IsString FeatureType where
  fromString = Auto . fromString

data RawData = RawData
    { names      :: [Text]
    , dataColumn :: Text -> Either ParseError (NonEmpty Text)
    }

type Scaling           = (Double, Double)
type ScaleStrategy     = Vector Double -> Scaling

-- there is some duplication, columns are part of the feature, but we're just
-- exposing functions over our data so it's probably ok to just provide both.
data Dataset = Dataset
    { dsName'      :: T.Text
    , dsFeatures   :: [Feature']
    , dsNumRows'   :: Int
    , dsNumCols'   :: Int
    , colByName'   :: T.Text  -> Maybe (Vector Double)
    , featByName   :: T.Text  -> Maybe Feature'
    , featureSpace :: FeatureSpace
    }

instance Show Dataset where
  show = toString . unlines . summary

-- meta data for a data set, defining the subset of the data that's
-- considered active in a given context (features available in a
-- dataset, features that where used to train a model,
-- result for variable selection algorithm, etc).
data FeatureSpace = FeatureSpace
    { findFeature :: Text -> Maybe Metadata
    , knownFeats  :: [Metadata]
    , ignoredCols :: [Text] -- TODO: gone with it
    }

instance Show FeatureSpace where
  show fs = GHC.Show.show (knownFeats fs) <> " | ignored = "
         <>  GHC.Show.show (ignoredCols fs)

data Feature' = Feature'
    { metadata :: Metadata
    , columns  :: [Vector Double] } deriving (Show, Eq)

data Metadata =
    Categorical'
        { featName' :: Text
        , baseLabel :: Text
        , otherLabels :: [Text]
        } |
    Continuous'
        { featName' :: Text
        , offset    :: Double
        , scale     :: Double } deriving (Show, Eq, Ord)

instance Summary Feature' where
  summary (Feature' (Continuous' fn sh sc) [col]) = [summarizeVector fn col]
  summary (Feature' (Categorical' fn bs ol) cols) =
      let size           = fromIntegral $ maybe 0 V.length $ listToMaybe cols
          baseFeat       = baselineColumn cols
          fc :: Text -> Vector Double -> Text
          fc name xs = sformat (textF 5 % ": n=" % intF % " " % percF % "%")
            name
            (round $ V.sum xs)
            (dSc $ 100.0*(V.sum xs)/size)
          texts = uncurry fc <$> (bs,baseFeat):(zip ol cols)
    in [sformat (textF 13) fn <> (T.intercalate " | " $ texts)]


instance Summary Dataset where
  summary ds = concatMap summary $ dsFeatures ds

parseDataset :: [FeatureType] -> RawData -> Either ParseError Dataset
parseDataset fs RawData {..} =
    let feature ft = createFeature' ft <$> dataColumn (name ft)
        features   = traverse feature fs
    in createFromFeatures "unknown dataset" <$> features

parseFullDataset :: RawData -> Either ParseError Dataset
parseFullDataset rd =
    let features = Auto <$> names rd
    in parseDataset features rd

createFromFeatures :: T.Text -> [Feature'] -> Dataset
createFromFeatures name feats =
    let dsName'      = name
        dsFeatures   = sortOn featureName feats
        featureSpace = createFeatureSpace $ metadata <$> dsFeatures
        columnsInSet = concatMap columns dsFeatures
        dsNumRows'   = fromMaybe 0 (V.length <$> (listToMaybe columnsInSet))
        dsNumCols'   = length columnsInSet
        columnIndex  = M.fromList $ zip (columnNames featureSpace) columnsInSet
        featureIndex = M.fromList $ zip (featureName <$> dsFeatures) dsFeatures
        featByName f = M.lookup f featureIndex
        findMissingClass :: T.Text -> Maybe (Vector Double)
        findMissingClass (splitFeatureName -> Just (feat, klass)) =
            featByName feat >>= \case
                Feature' (Continuous' _ _ _) _ -> Nothing
                (Feature' (Categorical' _ baseFeature _) vss) ->
                    if baseFeature == klass
                       then return $ baselineColumn vss
                       else return $ V.replicate dsNumRows' 0.0
        findMissingClass _ = Nothing
        colByName' c = whenNothing (M.lookup c columnIndex) (findMissingClass c)
    in Dataset { .. }

extractDataColumns :: Dataset -> FeatureSpace -> [Vector Double]
extractDataColumns ds FeatureSpace { .. } =
    concatMap (featureVectors' ds) knownFeats

featureVectors' :: Dataset -> Metadata -> [Vector Double]
featureVectors' ds Categorical' {..} =
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

featureName :: Feature' -> Text
featureName = featName' . metadata

baselineColumn :: [Vector Double] -> Vector Double
baselineColumn vss =
    let n      = maybe 0 V.length $ listToMaybe vss
        vsum i = sum $ (V.! i) <$> vss
    in  V.generate n (\i -> 1.0 - vsum i) :: Vector Double

createFeature' :: FeatureType -> NonEmpty Text -> Feature'
createFeature' (Auto name) xs@(x :| rest) =
    let parseFirst = readEither x :: Either Text Double
    in case parseFirst of
        Right _ -> createContinuous undefined name xs
        Left _  -> createCategorical'  name xs

createContinuous :: ScaleStrategy -> Text -> NonEmpty Text -> Feature'
createContinuous _sc name (x:|xs) =
    let vs = replaceNAs $ parseNumbers (x:xs)
    in Feature' (Continuous' name 0 1) [vs]

createCategorical' :: Text -> NonEmpty Text -> Feature'
createCategorical' name xs =
    let klasses                = NE.sort $ NE.nub xs
        (baseFeature, mOthers) = NE.uncons klasses
        others                 = fromMaybe [] (NE.toList <$> mOthers)
        features               = fmap createKlassVector others
        createKlassVector kl   = V.fromList $
            fmap (\d -> if d == kl then 1.0 else 0.0 :: Double) $ NE.toList xs
        metadata = (Categorical' name baseFeature others)
    in Feature' metadata $ createKlassVector <$> others


replaceNAs :: Vector Double -> Vector Double
replaceNAs xs =
    let mean      = C.vmean $ V.filter (not . isNaN) xs
    in V.map (\x -> if isNaN x then mean else x) xs

parseNumbers :: [Text] -> V.Vector Double
parseNumbers xs =
    let fallback = sqrt $ -1
    in replaceNAs $ V.fromList $ fromRight fallback . readEither <$> xs

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
featColNames Continuous' {..}  = [featName']
featColNames Categorical' {..} = ((featName' <> "_") <>) <$> otherLabels
