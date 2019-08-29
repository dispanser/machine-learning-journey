{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE PartialTypeSignatures#-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Very simple prototype of how to represent data
goals:
    - as simple as possible
    - regression only
    - everything's a double

ideas:
    - extractFeatureVector should produce an Either, indicating what went wrong
      (or some other kind of validation approach)
    - create a formula language similar to R, where "sales ~ tv + radio + ..."
      - alternatively, more of a DSL, e.g.: "sales" onto "tv" plus "radio" ...
    - combine this formula language into jjj

-}

module ISL.DataSet where

import           Control.Monad.ST (runST, ST)
import qualified Data.Map.Strict as M
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Text as T
import           Data.Text (Text)

class Predictor a where
  predict :: a -> [Column Double] -> Column Double

class Summary a where
  summary :: a -> Text

class ModelFit a where
  fit :: ModelInput -> a

-- represents the input data, i.e. the housing dataset in its raw form
data DataSet = DataSet
    { dsName          :: Text
    , dsColumnIndices :: M.Map Text Int
    , dsColumnData    :: [Vector Double]
    } deriving        (Show)

-- TODO: better name, please
-- represents the input to a fit procedure: a data set and the
-- names of the features, the name of the response etc.
-- at this stage, the inputs are verified
data ModelInput = ModelInput
    { miName     :: !Text
    , miFeatures :: ![Column Double]
    , miResponse :: Column Double } deriving (Show, Eq)

-- a column is just a named vector
data Column a = Column
    { colName :: Text
    , colData :: Vector a } deriving (Eq, Ord)

instance Show a => Show (Column a) where
  show Column { .. } = "Col(" ++ T.unpack colName ++ "=" ++ show colData ++ ")"

-- split the training input so that we can use one piece as training, the
-- other part for validation.
splitModelInput :: [Bool] -> ModelInput -> (ModelInput, ModelInput)
splitModelInput testRows modelInput = (train, test)
 where (trainResponse, testResponse) = splitColumn testRows $ miResponse modelInput
       (trainFeatures, testFeatures) = unzip $ splitColumn testRows <$> miFeatures modelInput
       train = ModelInput
           { miName = miName modelInput <> "_train"
           , miFeatures = trainFeatures
           , miResponse = trainResponse }
       test  = ModelInput
           { miName = miName modelInput <> "_test"
           , miFeatures = testFeatures
           , miResponse = testResponse }

splitColumn :: [Bool] -> Column Double -> (Column Double, Column Double)
splitColumn idxs Column {..} =
    let (leftV, rightV) = splitVector idxs colData
    in (Column colName leftV, Column colName rightV)

-- this actually prompted for a call for help on r/haskell:
-- https://www.reddit.com/r/haskell/comments/ckba3b/monthly_hask_anything_august_2019/eybwyig/
-- the problem is that runST introduces an s (for ST s), but the step function introduces
-- another, incompatible one. So the VM.write call wouldn't compile. Solution is to introduce
-- ScopedTypeVariables, have an explicit type signature for the runST (go), bind some s using
-- forall, and then re-using that s in the type signature of the step function.
-- after all these changes, it became possible drop the type signature for step:
splitVector :: [Bool] -> Vector Double -> (Vector Double, Vector Double)
splitVector idxs v =
    runST go
      where
        n         = V.length v
        leftSize  = length (filter id $ take n idxs)
        rightSize = n - leftSize
        go :: forall s . ST s (Vector Double, Vector Double)
        go = do
            lefts  <- VM.new leftSize
            rights <- VM.new rightSize
            let --  step :: Int -> Int -> Int -> ST s () -- no longer necessary, uhm.
                step _ _ []     = return ()
                step l r ((p,i):ps) =
                    if p
                       then VM.write lefts  l (v ! i) >> step (l+1) r ps
                       else VM.write rights r (v ! i) >> step l (r+1) ps
            step 0 0 $ zip idxs [0 .. n-1]
            (,) <$> V.freeze lefts <*> V.freeze rights

extractFeatureVector :: Text -> DataSet -> Maybe (Column Double)
extractFeatureVector colName DataSet { .. } =
    Column colName . (dsColumnData !!) <$> M.lookup colName dsColumnIndices


extractFeatureVectors :: [Text] -> DataSet -> Maybe [Column Double]
extractFeatureVectors colNames ds = traverse (flip extractFeatureVector ds) colNames

extractModelInput :: Text -> [Text] -> DataSet -> Maybe ModelInput
extractModelInput responseName featureNames ds@DataSet { .. }  = do
    featureCols <- extractFeatureVectors featureNames ds
    responseCol <- extractFeatureVector  responseName ds
    return ModelInput
        { miName     = dsName
        , miFeatures = featureCols
        , miResponse = responseCol }

