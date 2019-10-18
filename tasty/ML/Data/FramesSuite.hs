{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, OverloadedStrings  #-}

module ML.Data.FramesSuite where

import           Base
import           Test.Tasty.Hspec (Spec)
import           Test.Hspec
import           Lens.Micro.Extras
import           Frames (Frame)
import qualified Frames as F
import qualified Data.Vector as V
import qualified Data.Foldable as Fl
import qualified Statistics.Sample as S

import           ML.Data.Frames  (normalizeColumn)
import           ML.Data.Sources (advertisingFilepath)

F.tableTypes "Adv" advertisingFilepath

F.declareColumn "Cat1" ''String
F.declareColumn "ContDouble" ''Double
F.declareColumn "ContInt" ''Int

spec_NormalizeColumn :: Spec
spec_NormalizeColumn =
    describe "normalized a column in a frame" $ do
        advFrame <- normalizeColumn <$> runIO loadAdvertising
        let salesData        = V.fromList . Fl.toList $ view sales <$> advFrame
            (mean, variance) = S.meanVarianceUnb salesData
        it "should have mean zero" $
            mean `shouldRoughlyEqual` 0
        it "should have variance 1" $
            variance `shouldRoughlyEqual` 1

loadAdvertising :: IO (Frame Adv)
loadAdvertising = F.inCoreAoS $ F.readTable advertisingFilepath
