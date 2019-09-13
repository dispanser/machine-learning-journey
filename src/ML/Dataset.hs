{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-
Very simple prototype of how to represent data

This is far away from being usable:
- we assume everything's a double
- no support for categorical features
- no support for N/A
-}

module ML.Dataset where

import           GHC.Show (Show(..))
import qualified Data.Map.Strict as M
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as VG
import qualified Formatting as F
import qualified Formatting.ShortFormatters as F
import           Formatting ((%), (%.))
import qualified Statistics.Quantile as Q
import qualified Statistics.Sample as S
import           Statistics.Function (minMax)

class Summary a where
  summary :: a -> [Text]

-- select the subset of rows that are
type RowSelector = Int -> Bool

type ColumnTransformer = Column Double -> Column Double

-- there is some duplication, columns are part of the feature, but we're just
-- exposing functions over our data so it's probably ok to just provide both.
data Dataset = Dataset
    { dsName'      :: T.Text
    , dsFeatures   :: [Feature Double]
    , dsColumns'   :: [Column Double]
    , dsNumRows'   :: Int
    , dsNumCols'   :: Int
    , colByName'   :: T.Text  -> Maybe (Column Double)
    , featByName   :: T.Text  -> Maybe (Feature Double)
    , featureSpace :: FeatureSpace
    }

instance Show Dataset where
  show = toString . unlines . summary

-- meta data for a data set, defining the subset of the data that's
-- considered active in a given context (features available in a
-- dataset, features that where used to train a model,
-- result for variable selection algorithm, etc).
data FeatureSpace = FeatureSpace
    { findFeature :: Text -> Maybe FeatureSpec
    , knownFeats  :: [FeatureSpec]
    , ignoredCols :: [Text]
    }

instance Show FeatureSpace where
  show fs = GHC.Show.show (knownFeats fs) <> " | "
         <>  GHC.Show.show (ignoredCols fs)

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

data ScaledColumn  = ScaledColumn
    { rawColumn   :: Column Double
    , scaleOffset :: Double
    , scaleFactor :: Double
    } deriving Show

instance Summary (Feature Double) where
  summary (SingleCol col) = summary col
  summary (MultiCol cat ) = summary cat

instance Summary Dataset where
  summary ds = concatMap summary $ dsFeatures ds

instance Summary (Column Double) where
  summary = (:[]) . summarizeColumn

instance Summary (Categorical Double) where
  summary = (:[]) . summarizeCategorical

createFromFeatures :: T.Text -> [Feature Double] -> Dataset
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
    in Dataset { .. }

rescaleColumn :: Column Double -> ScaledColumn
rescaleColumn c@(colData -> xs) =
    let (minV, maxV) = minMax xs
        range        = maxV - minV
    in ScaledColumn
        { rawColumn   = c { colData =  VG.map (\x -> (x - minV) / range) xs}
        , scaleOffset = minV   -- -minV, to be pedantic
        , scaleFactor = range  -- 1/range, to be pedantic
        }

rescale01 :: VG.Vector v Double => v Double -> v Double
rescale01 xs =
    let (minV, maxV) = minMax xs
        range        = maxV - minV
    in VG.map (\x -> (x - minV) / range) xs

-- column variance.
columnVariance :: Column Double -> Double
columnVariance = S.varianceUnbiased . colData

columnLength :: V.Unbox a => [Column a] -> Int
columnLength []     = 0
columnLength (x:_) = V.length . colData $ x

-- TODO: Either Text ? notion of missing / spec mismatch?
extractDataColumns :: Dataset -> FeatureSpace -> [Column Double]
extractDataColumns ds FeatureSpace { .. } =
    let columns = concatMap (featureVectors' ds) $ knownFeats
    in filter (not . (`elem` ignoredCols) . colName) columns


filterDataColumn :: V.Unbox a => RowSelector -> Column a -> Column a
filterDataColumn rs (Column name cData) =
    Column name $ V.ifilter (\i _ -> rs i) cData

featureVectors' :: Dataset -> FeatureSpec -> [Column Double]
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

featureName :: Feature a -> Text
featureName (SingleCol Column { .. })      = colName
featureName (MultiCol  Categorical { .. }) = className

featureSize :: V.Unbox a => Feature a -> Int
featureSize (SingleCol col)                = colSize col
featureSize (MultiCol  Categorical { .. }) = maybe 0 colSize $ listToMaybe features

featureVectors :: Feature a -> [Vector a]
featureVectors (SingleCol Column { .. })     = [colData]
featureVectors (MultiCol Categorical { .. }) = colData <$> features

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

colSize :: V.Unbox a => Column a -> Int
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
    let mean      = vmean $ V.filter (not . isNaN) xs
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

summarizeColumn :: Column Double -> Text
summarizeColumn Column { .. } =
    let [min', fstQ, med, thrdQ, max'] =
            Q.quantiles Q.medianUnbiased [0..4] 4 colData
        mean = vmean colData
    in F.sformat (textF  13 % " Min: " % scieF % " 1stQ:" % scieF %
        " Med: " % scieF % " 3rdQ:" % scieF % " Max:" % scieF %
            " Mean:" % scieF)
            colName (dSc min') (dSc fstQ) (dSc med) (dSc thrdQ)
            (dSc max') (dSc mean)

dSc :: Double -> Scientific.Scientific
dSc = Scientific.fromFloatDigits

scieF, percF :: F.Format r' (Scientific.Scientific -> r')
scieF = F.left 11 ' ' %. F.scifmt Scientific.Generic (Just 2)
percF = F.left 5 ' ' %. F.scifmt Scientific.Generic (Just 2)

textF :: Int -> F.Format r' (Text -> r')
textF i = (F.l i ' ' %. F.st)

numF :: F.Format r' (Integer -> r')
numF  = F.l 13 ' ' %. (F.fitRight 13 %. F.sf)

intF :: F.Format r (Integer -> r)
intF = F.r 4 ' ' %. F.d

vmean :: VG.Vector v Double => v Double -> Double
vmean vs = VG.sum vs / fromIntegral (VG.length vs)

summarizeCategorical :: Categorical Double -> Text
summarizeCategorical c@Categorical { .. } =
    let size        = fromIntegral $ columnLength features
        featNameLength = succ $ T.length className
        baseFeat= baselineColumn className c
        fc :: Column Double -> Text
        fc Column { .. } = F.sformat (textF 5 % ": n=" % intF % " " % percF % "%")
            (T.drop featNameLength colName)
            (round $ V.sum colData)
            (dSc $ 100.0*(V.sum colData)/size)
        texts = fc <$> baseFeat:features
    in F.sformat (textF 13) className <> (T.intercalate " | " $ texts)

rowSelectorFromList :: [Bool] -> RowSelector
rowSelectorFromList xs =
    let v = V.fromList xs
    in (v V.!)

