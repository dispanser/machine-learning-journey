{-
    linear regression model solved via gradient descent.
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ML.LinearRegressionGD where

import           GHC.Show (Show(..))
import qualified Relude.Unsafe as RU
import qualified Data.List as DL
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (Vector)
import           ML.Data.Summary
import           ML.Dataset (Feature(..))
import qualified ML.Dataset as DS
import qualified ML.Model as M
import           ML.Model (ModelSpec(..), Predictor(..), ModelInit(..))
import qualified ML.LinearRegression as LR
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra (Matrix, (#>), (<#), (<.>))
import qualified Numeric.Morpheus.Statistics as MS

data LinearRegressionGD = LinearRegressionGD
    { lrCoefficients   :: Vector Double
    , lrTss            :: Double
    , lrRss            :: Double
    , lrDF             :: Int
    , n                :: Int
    , p                :: Int
    , cfg              :: ModelConfig
    } -- deriving Show

type LearningRate     = Double
type PenaltyWeight    = Double
type TrainingFinished = Pred TrainingState

-- to solve the gradient descent, the cost function is not necessary,
-- but it might be nice if you wanna plot cost functions over training
-- iterations or penalty weight or similar.
-- the penalty weight could also be integrated into the cost and derivatives
-- functions themselves, but it's nice to have it separated out.
data PenaltyTerm = PenaltyTerm
    { costFunction  :: Vector Double -> Vector Double
    , penaltyDeriv  :: Vector Double -> Vector Double
    , penaltyWeight :: PenaltyWeight
    }

data TrainingState = TrainingState
    { stepCoefficients :: Vector Double
    , rss          :: Double
    , dRss         :: Double
    , iter         :: Int } deriving Show

data ModelConfig = ModelConfig
    { learnRate   :: LearningRate
    , penaltyTerm :: PenaltyTerm
    , finishPred  :: TrainingFinished
    , lrModelSpec :: ModelSpec
    }

instance Show ModelConfig where
  show ModelConfig { .. } = "ModelConfig{α=" <> GHC.Show.show learnRate <>
      " spec=" <> GHC.Show.show lrModelSpec <>  "}"

instance Summary LinearRegressionGD where
  summary = summarizeLinearRegressionGD

instance LR.LinearModel LinearRegressionGD where
  coefficients    = lrCoefficients
  rss             = lrRss
  tss             = lrTss
  degreesOfFredom = fromIntegral . lrDF

instance M.Model LinearRegressionGD where
  modelSpec' = lrModelSpec . cfg

instance Predictor LinearRegressionGD where
  predict LinearRegressionGD { .. } cols  =
      let prediction = LR.predictLinearRegression lrCoefficients $ VS.convert <$> cols
      in Feature (response $ lrModelSpec cfg ) [VS.convert prediction]

linearRegressionGD :: LearningRate -> Int -> ModelSpec -> M.ModelInit LinearRegressionGD
linearRegressionGD a n ms =
    let cfg = ModelConfig a noPenalty (maxIterations n) ms
    in initLR cfg

ridgeRegression :: PenaltyWeight -> LearningRate -> Int -> ModelSpec -> M.ModelInit LinearRegressionGD
ridgeRegression pw a n ms =
    let cfg = ModelConfig a (ridgePenalty pw) (maxIterations n) ms
    in initLR cfg

lassoRegression :: PenaltyWeight -> LearningRate -> Int -> ModelSpec -> M.ModelInit LinearRegressionGD
lassoRegression pw a n ms =
    let cfg = ModelConfig a (lassoPenalty pw) (maxIterations n) ms
    in initLR cfg

initLR :: ModelConfig -> M.ModelInit LinearRegressionGD
initLR cfg = M.ModelInit
    { fitF        = fitLinearRegression cfg
    , modelSpec = lrModelSpec cfg }

fitLinearRegression :: ModelConfig -> M.FitF LinearRegressionGD
fitLinearRegression cfg inputCols response =
    let y  = VS.convert response
        n  = VS.length y
        xs = VS.convert <$> inputCols :: [Vector Double]
        xX = LR.prepareMatrix n xs
        p  = pred $ LA.cols xX
        yMean            = MS.mean y
        yDelta           = y - (VS.replicate n yMean)
        lrTss            = yDelta <.> yDelta
        lrDF             = n - p - 1
        step'            = step (learnRate cfg) (penaltyTerm cfg) xX y
        initialState     = TrainingState (VS.replicate (succ p) 0.0) lrTss lrTss 0
        stateSeq         = iterate step' initialState
        finalState       = RU.head $ dropWhile (finishPred cfg) stateSeq
        lrCoefficients   = stepCoefficients finalState
        residuals        = y - LR.predictLinearRegression lrCoefficients xs
        lrRss            = residuals <.> residuals
    in LinearRegressionGD { .. }

step :: LearningRate
     -> PenaltyTerm
     -> Matrix Double  -- X
     -> Vector Double  -- Y
     -> TrainingState   -- theta
     -> TrainingState   -- theta'
step a pt xX y TrainingState { .. } =
    let yHat = xX #> stepCoefficients
        resi = yHat - y
        nRss = resi <.> resi
        derivatives = if iter `mod` 1000000 == 999999
                         then (debugShow "parD beta" $ (yHat - y) <# xX)
                                 + (debugShow "penalty" $ LA.scale (penaltyWeight pt) ((penaltyDeriv pt) stepCoefficients))
                         else (yHat - y) <# xX
                                 + LA.scale (penaltyWeight pt) ((penaltyDeriv pt) stepCoefficients)

    in  --debugShow "iter" $
        TrainingState
            { stepCoefficients = stepCoefficients - (LA.scale a derivatives)
            , iter         = iter + 1
            , rss          = nRss
            , dRss         = rss - nRss }

-- signals true iff the rss change between two consecutive training states is smaller
-- than the given threshold
rssDeltaBelow :: Double -> TrainingFinished
rssDeltaBelow threshold (dRss -> dRss) = threshold < abs dRss

maxIterations :: Int -> TrainingFinished
maxIterations n (iter -> n1) = n >= n1

noPenalty :: PenaltyTerm
noPenalty =
    let zeroF xs = VS.replicate (VS.length xs) 0
    in PenaltyTerm zeroF zeroF 0

-- helper that allows skipping the intercept (index 0) for upcoming functions.
-- intercept is usually not incorporated into the cost function, as we don't
-- want to minimize its value.
noIntercept :: (Double -> Double) -> Int -> Double -> Double
noIntercept _ 0 _ = 0
noIntercept f _ x = f x

ridgePenalty :: PenaltyWeight -> PenaltyTerm
ridgePenalty l =
    let costF  = VS.imap (noIntercept (^(2::Int)))
        derivF = VS.imap (noIntercept (2*))
    in PenaltyTerm costF derivF l

lassoPenalty :: PenaltyWeight -> PenaltyTerm
lassoPenalty l =
    let costF  = VS.imap (noIntercept identity)
        derivF = VS.imap (noIntercept signum)
    in PenaltyTerm costF derivF l

summarizeLinearRegressionGD :: LinearRegressionGD -> [Text]
summarizeLinearRegressionGD lr@LinearRegressionGD { .. } =
    let ms           = lrModelSpec cfg
        featureNames = DS.columnNames $ features' ms
    in [ LR.formatFormula
        (M.responseName ms)
        featureNames
       , "Feature           | coefficient | scaled coef "
       , "------------------+-------------+-------------"
       ] ++
           (DL.zipWith3 formatCoefficientInfo
            ("Intercept" : featureNames)
            (VS.toList lrCoefficients)
            (LR.recoverOriginalCoefficients lr))
              ++
                [ ""
                , sformat ("R^2         : " % fixed 4) (LR.r2 lr)
                , sformat ("F-Statistics: " % fixed 4) $ LR.fStatistics lr]


formatCoefficientInfo :: Text -> Double -> Double -> Text
formatCoefficientInfo name coef scCoef =
    let formatString = (left 17 ' ' %. stext) % " | " % floatF 10 5 % " | " % floatF 10 5
    in sformat formatString name
        (coef)
        (scCoef)
