{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE PartialTypeSignatures#-}
{-# LANGUAGE ScopedTypeVariables #-}

module ISL.Model where

import qualified Relude.Unsafe as RU
import           ISL.DataSet (DataSet(..))
import           Control.Monad.ST (runST, ST)
import qualified Data.Map.Strict as M
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Text (Text)

class Predictor a where
  predict :: a -> [Feature Double] -> Feature Double

class ModelFit a where
  fit :: ModelInput -> a

-- TODO: better name, please
-- represents the input to a fit procedure: a data set and the
-- names of the features, the name of the response etc.
-- at this stage, the inputs are verified
data ModelInput = ModelInput
    { miName     :: !Text
    , miFeatures :: ![Feature Double]
    , miResponse :: Feature Double
    , miRows     :: Int } deriving (Show, Eq)

data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Eq, Show)

data Categorical a = Categorical
    { className   :: Text
    , baseFeature :: Text
    , features    :: [Column a] } deriving (Eq, Show)

-- a feature is just a named vector
data Feature a = SingleCol (Column a)
               | MultiCol  (Categorical a) deriving (Eq, Show)

featureName :: Feature a -> Text
featureName (SingleCol Column { .. })      = colName
featureName (MultiCol  Categorical { .. }) = className

featureSize :: Feature a -> Int
featureSize (SingleCol col)                = colSize col
featureSize (MultiCol  Categorical { .. }) = colSize $ RU.head features

featureVectors :: Feature a -> [Vector a]
featureVectors (SingleCol Column { .. })     = [colData]
featureVectors (MultiCol Categorical { .. }) = colData <$> features

featureVector :: Feature a -> Vector a
featureVector (SingleCol Column { .. })     = colData
featureVector (MultiCol Categorical { .. }) = undefined

-- inputVectors :: ModelInput -> [Vector a]
-- inputVectors ModelInput { .. } = undefined

colSize :: Column a -> Int
colSize = V.length . colData

-- split the training input so that we can use one piece as training, the
-- other part for validation.
splitModelInput :: [Bool] -> ModelInput -> (ModelInput, ModelInput)
splitModelInput testRows modelInput = (train, test)
 where (trainResponse, testResponse) = splitFeature testRows $ miResponse modelInput
       (trainFeatures, testFeatures) = unzip $ splitFeature testRows <$> miFeatures modelInput
       train = ModelInput
           { miName     = miName modelInput <> "_train"
           , miFeatures = trainFeatures
           , miResponse = trainResponse
           , miRows     = featureSize trainResponse }
       test  = ModelInput
           { miName     = miName modelInput <> "_test"
           , miFeatures = testFeatures
           , miResponse = testResponse
           , miRows     = featureSize testResponse }

splitFeature :: [Bool] -> Feature a -> (Feature a, Feature a)
splitFeature idxs (SingleCol Column { .. }) =
    let (leftV, rightV) = splitVector idxs colData
    in (SingleCol (Column colName leftV), SingleCol (Column colName rightV))
splitFeature idxs (MultiCol Categorical { .. }) =
    let (leftF, rightF) = unzip $ splitColumn idxs <$> features
    in (MultiCol (Categorical className baseFeature leftF), MultiCol (Categorical className baseFeature rightF))

splitColumn :: [Bool] -> Column a -> (Column a, Column a)
splitColumn idxs Column { .. } =
    let (leftV, rightV) = splitVector idxs colData
    in (Column colName leftV, Column colName rightV)

-- this actually prompted for a call for help on r/haskell:
-- https://www.reddit.com/r/haskell/comments/ckba3b/monthly_hask_anything_august_2019/eybwyig/
-- the problem is that runST introduces an s (for ST s), but the step function introduces
-- another, incompatible one. So the VM.write call wouldn't compile. Solution is to introduce
-- ScopedTypeVariables, have an explicit type signature for the runST (go), bind some s using
-- forall, and then re-using that s in the type signature of the step function.
-- after all these changes, it became possible drop the type signature for step:
splitVector :: forall a . [Bool] -> Vector a -> (Vector a, Vector a)
splitVector idxs v =
    runST go
      where
        n         = V.length v
        leftSize  = length (filter identity $ take n idxs)
        rightSize = n - leftSize
        go :: forall s . ST s (Vector a, Vector a)
        go = do
            lefts'  <- VM.new leftSize
            rights' <- VM.new rightSize
            let --  step :: Int -> Int -> Int -> ST s () -- no longer necessary, uhm.
                step _ _ []     = return ()
                step l r ((p,i):ps) =
                    if p
                       then VM.write lefts'  l (v ! i) >> step (l+1) r ps
                       else VM.write rights' r (v ! i) >> step l (r+1) ps
            step 0 0 $ zip idxs [0 .. n-1]
            (,) <$> V.freeze lefts' <*> V.freeze rights'

extractFeatureVector :: Text -> DataSet -> Maybe (Feature Double)
extractFeatureVector colName DataSet { .. } =
    let fallback = sqrt $ -1
    in SingleCol <$> Column colName <$> V.map (either (const fallback) identity . readEither) <$> M.lookup colName dsColumnIndices

extractFeatureVectors :: [Text] -> DataSet -> Maybe [Feature Double]
extractFeatureVectors colNames ds = traverse (flip extractFeatureVector ds) colNames

extractModelInput :: Text -> [Text] -> DataSet -> Maybe ModelInput
extractModelInput responseName featureNames ds@DataSet { .. }  = do
    featureCols <- extractFeatureVectors featureNames ds
    responseCol <- extractFeatureVector  responseName ds
    return ModelInput
        { miName     = dsName
        , miFeatures = featureCols
        , miResponse = responseCol
        , miRows     = featureSize responseCol}

