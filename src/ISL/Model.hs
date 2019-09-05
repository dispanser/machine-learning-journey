{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE PartialTypeSignatures#-}
{-# LANGUAGE ScopedTypeVariables #-}

module ISL.Model where

import           ML.DataSet (DataSet'(..), Feature(..), Column(..)
                            , Categorical(..), FeatureSpace(..)
                            , FeatureSpec(..), featureSize
                            , createFeature, createFeatureSpace)
import           ISL.DataSet (DataSet(..))
import           Control.Monad.ST (runST, ST)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Text (Text)

-- initial stub type for prediction result: the column vector of response
type Prediction = Feature Double

class Predictor a where
  predict  :: a -> [Feature Double] -> Feature Double
  predict' :: a -> DataSet'         -> Prediction

class ModelFit a where
  fit  :: ModelInput -> a
  fit' :: DataSet' -> ModelSpec -> a

data ModelSpec = ModelSpec
    { features' :: FeatureSpace
    , response  :: FeatureSpec }

-- TODO: better name, please
-- represents the input to a fit procedure: a data set and the
-- names of the features, the name of the response etc.
-- at this stage, the inputs are verified (in theory)
data ModelInput = ModelInput
    { miName     :: !Text
    , miFeatures :: ![Feature Double]
    , miResponse :: Feature Double
    , miRows     :: Int } deriving (Show, Eq)

modelSpecByFeatureNames :: FeatureSpace -> Text -> [Text] -> Either Text ModelSpec
modelSpecByFeatureNames FeatureSpace {..} responseName featureNames = do
    response <- maybe (Left $ "response named '" <> responseName <> "' not found")
        Right $ findFeature responseName
    features <- forM featureNames (\fn -> maybe (Left $ "feature named '" <> fn <> "' not found")
                   Right $ findFeature fn)
    return $ ModelSpec
        { features' = createFeatureSpace features
        , response  = response }

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

extractModelInput :: Text -> [Text] -> DataSet -> Maybe ModelInput
extractModelInput responseName names ds@DataSet { .. }  = do
    featureCols <- extractFeatureVectors ds names
    responseCol <- extractFeatureVector  ds responseName
    return ModelInput
        { miName     = dsName
        , miFeatures = featureCols
        , miResponse = responseCol
        , miRows     = featureSize responseCol }

extractFeatureVector :: DataSet -> Text -> Maybe (Feature Double)
extractFeatureVector DataSet { .. } colName = createFeature colName <$> colByName colName

extractFeatureVectors :: DataSet -> [Text] -> Maybe [Feature Double]
extractFeatureVectors ds colNames = traverse (extractFeatureVector ds) colNames

