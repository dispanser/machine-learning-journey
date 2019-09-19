{-# LANGUAGE OverloadedStrings #-}

module ML.Data.Feature.Internal
    ( FeatureType  (..)
    , Feature'     (..)
    , FeatureSpace (..)
    , Metadata     (..)
    , baselineColumn
    , createFeature'
    , featureName
    , createCategorical'
    ) where

import           GHC.Show (Show(..))
import           ML.Data.Vector (summarizeVector, replaceNAs, parseNumbers)
import           ML.Data.Summary
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type Scaling           = (Double, Double)
type ScaleStrategy     = Vector Double -> Scaling

data FeatureType =
    Auto { name :: Text }
      | Cat { name :: Text }
      | Cont { name :: Text }

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

data Feature' = Feature'
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

instance Show FeatureSpace where
  show fs = GHC.Show.show (knownFeats fs) <> " | ignored = "
         <>  GHC.Show.show (ignoredCols fs)

createFeature' :: FeatureType -> NonEmpty Text -> Feature'
createFeature' (Auto name) xs@(x :| rest) =
    let parseFirst = readEither x :: Either Text Double
    in case parseFirst of
        Right _ -> createContinuous undefined name xs
        Left _  -> createCategorical'  name xs

featureName :: Feature' -> Text
featureName = featName' . metadata

baselineColumn :: [Vector Double] -> Vector Double
baselineColumn vss =
    let n      = maybe 0 V.length $ listToMaybe vss
        vsum i = sum $ (V.! i) <$> vss
    in  V.generate n (\i -> 1.0 - vsum i) :: Vector Double

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

