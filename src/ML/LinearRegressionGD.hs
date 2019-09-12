{-
    linear regression model solved via gradient descent.
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ML.LinearRegressionGD where

import qualified Relude.Unsafe as RU
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (Vector)
import           ML.Dataset (Feature(..), Column(..))
import qualified ML.Model as MM
import           ML.Model (ModelSpec(..), Predictor(..))
import qualified ML.LinearRegression as OLS
import qualified Numeric.LinearAlgebra as M
import           Numeric.LinearAlgebra (Matrix, (#>), (<#), (<.>))
import qualified Numeric.Morpheus.Statistics as MS

data LinearRegressionGD = LinearRegressionGD
    { lrFeatureNames   :: [T.Text]
    , lrResponseName   :: T.Text
    , lrCoefficients   :: Vector Double
    , lrStandardErrors :: Vector Double
    , lrRse            :: Double
    , lrR2             :: Double
    , lrTss            :: Double
    , lrRss            :: Double
    , lrDF             :: Int
    , n                :: Int
    , p                :: Int
    , lrModelSpec      :: ModelSpec -- TODO: remove!
    , cfg              :: ModelConfig
    }

type LearningRate = Double

type TrainingFinished = Pred TrainingState

data TrainingState = TrainingState
    { coefficients :: Vector Double
    , rss          :: Double
    , dRss         :: Double
    , iter         :: Int } deriving Show

data ModelConfig = ModelConfig
    { learnRate  :: LearningRate
    , finishPred :: TrainingFinished
    , modelSpec  :: ModelSpec
    }

instance Predictor LinearRegressionGD where
  predict   LinearRegressionGD { .. } cols  =
      SingleCol . Column lrResponseName $ VS.convert $
          OLS.predictLinearRegression lrCoefficients $
              VS.convert . colData <$> cols
  features = features' . lrModelSpec

type Coefficients = Vector Double

linearRegressionGD :: ModelConfig -> MM.ModelInit LinearRegressionGD
linearRegressionGD cfg = MM.ModelInit
    { fitF      = flip (fitLinearRegression cfg)
    , modelSpec = modelSpec cfg }

fitLinearRegression :: ModelConfig
                    -> Column Double
                    -> [Column Double]
                    -> LinearRegressionGD
fitLinearRegression cfg response inputCols =
    let y  = VS.convert . colData $ response
        n  = VS.length y
        xs = VS.convert . colData <$> inputCols :: [Vector Double]
        xX = OLS.prepareMatrix n xs
        p  = pred $ M.cols xX
        yDelta           = y - (VS.replicate n yMean)
        lrTss            = yDelta <.> yDelta
        lrDF             = n - p - 1
        step'            = step (learnRate cfg) xX y
        initialState     = TrainingState (VS.replicate (succ p) 0.0) lrTss lrTss 0
        stateSeq         = iterate step' initialState
        finalState       = RU.head $ dropWhile (finishPred cfg) stateSeq
            -- dropWhile (rssDeltaBelow 0.0000000003) stateSeq
        lrCoefficients   = coefficients finalState
        residuals        = y - OLS.predictLinearRegression lrCoefficients xs
        lrRss            = residuals <.> residuals
        yMean            = MS.mean y
        lrMse            = lrRss / fromIntegral (n - p - 1)
        lrRse            = sqrt lrMse
        lrStandardErrors = M.takeDiag $ M.scale lrMse (
            M.inv . M.unSym $ M.mTm xX) ** 0.5
        lrR2             = 1 - lrRss/lrTss
        lrResponseName   = colName response
        lrFeatureNames   = colName <$> inputCols
        lrModelSpec      = modelSpec cfg
    in LinearRegressionGD { .. }

step :: LearningRate
     -> Matrix Double  -- X
     -> Vector Double  -- Y
     -> TrainingState   -- theta
     -> TrainingState   -- theta'
step a xX y TrainingState { .. } =
    let yHat = xX #> coefficients
        resi = yHat - y
        nRss = resi <.> resi
    in  --debugShow "iter" $
        TrainingState
            { coefficients = coefficients - (M.scale a $ (yHat - y) <# xX)
            , iter         = iter + 1
            , rss          = nRss
            , dRss         = rss - nRss}

-- signals true iff the rss change between two consecutive training states is smaller
-- than the given threshold
rssDeltaBelow :: Double -> TrainingFinished
rssDeltaBelow threshold (dRss -> dRss) = threshold < abs dRss

maxIterations :: Int -> TrainingFinished
maxIterations n (iter -> n1) = n >= n1
