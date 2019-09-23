{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Feature.Internal
    ( FeatureType  (..)
    , Feature     (..)
    , FeatureSpace (..)
    , Metadata     (..)
    , Scaling
    , ScaleStrategy
    , baselineColumn
    , createFeature
    , featureName
    , createCategorical
    , auto
    , scaled01
    , scaled
    , noScaling
    ) where

import qualified GHC.Show as GS
import           ML.Data.Vector ( noScaling, summarizeVector, replaceNAs
                                , parseNumbers , scale01, scaleWith)
import           ML.Data.Summary
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type Scaling           = (Double, Double)
type ScaleStrategy     = Vector Double -> Scaling
type FeatureConstructor = Text -> FeatureType

data FeatureType =
    Auto     { name :: Text }
      | Cat  { name :: Text }
      | Cont { name :: Text
             , scaleStrategy :: ScaleStrategy }

data Metadata =
    Categorical
        { featName' :: Text
        , baseLabel :: Text
        , otherLabels :: [Text]
        } |
    Continuous
        { featName' :: Text
        , scaling   :: Scaling
        } deriving (Show, Eq, Ord)

data Feature = Feature
    { metadata :: Metadata
    , columns  :: [Vector Double] } deriving (Show, Eq)

-- meta data for a data set, defining the subset of the data that's
-- considered active in a given context (features available in a
-- dataset, features that where used to train a model,
-- result for variable selection algorithm, etc).
data FeatureSpace = FeatureSpace
    { findFeature :: Text -> Maybe Metadata
    , knownFeats  :: [Metadata]
    , ignoredCols :: [Text] -- TODO: gone with it
    }

-- instantiate a string into a feature type using auto-detection mechanism
instance IsString FeatureType where
  fromString = Auto . fromString

instance Summary Feature where
  summary (Feature (Continuous fn sc) [col]) = [summarizeVector fn sc col]
  summary (Feature (Categorical fn bs ol) cols) =
      let size           = fromIntegral $ maybe 0 V.length $ listToMaybe cols
          baseFeat       = baselineColumn cols
          fc :: Text -> Vector Double -> Text
          fc name' xs = sformat (textF 5 % ": n=" % intF % " " % percF % "%")
            name'
            (round $ V.sum xs)
            (dSc $ 100.0*(V.sum xs)/size)
          texts = uncurry fc <$> (bs,baseFeat):(zip ol cols)
    in [sformat (textF 13) fn <> (T.intercalate " | " $ texts)]
  -- invalid case: cont feat w/ multiple columns
  summary (Feature (Continuous fn _) xss) =
      ["invalid coding for feature '" <> fn <>
        "': should be a single column but got " <> show (length xss) <> " columns"]

instance Show FeatureSpace where
  show fs = GS.show (knownFeats fs) <> " | ignored = "
         <>  GS.show (ignoredCols fs)

createFeature :: FeatureType -> NonEmpty Text -> Feature
createFeature (Auto name') xs@(x :| _) =
    let parseFirst = readEither x :: Either Text Double
    in case parseFirst of
        Right _ -> createContinuous noScaling name' xs
        Left _  -> createCategorical  name' xs
createFeature (Cont n s) xs = createContinuous s n xs
createFeature (Cat  n)  xs  = createFeature (Auto n) xs

featureName :: Feature -> Text
featureName = featName' . metadata

baselineColumn :: [Vector Double] -> Vector Double
baselineColumn vss =
    let n      = maybe 0 V.length $ listToMaybe vss
        vsum i = sum $ (V.! i) <$> vss
    in  V.generate n (\i -> 1.0 - vsum i) :: Vector Double

createContinuous :: ScaleStrategy -> Text -> NonEmpty Text -> Feature
createContinuous ss name' (x:|xs) =
    let vsRaw = replaceNAs $ parseNumbers (x:xs)
        sc    = ss vsRaw
        vs    = scaleWith sc vsRaw
    in Feature (Continuous name' sc) [vs]

createCategorical :: Text -> NonEmpty Text -> Feature
createCategorical name' xs =
    let klasses                = NE.sort $ NE.nub xs
        (baseFeature, mOthers) = NE.uncons klasses
        others                 = fromMaybe [] (NE.toList <$> mOthers)
        createKlassVector kl   = V.fromList $
            fmap (\d -> if d == kl then 1.0 else 0.0 :: Double) $ NE.toList xs
        md = (Categorical name' baseFeature others)
    in Feature md $ createKlassVector <$> others

auto :: FeatureConstructor
auto = Auto

scaled01 :: FeatureConstructor
scaled01 = scaled scale01

scaled :: ScaleStrategy -> FeatureConstructor
scaled = flip Cont
