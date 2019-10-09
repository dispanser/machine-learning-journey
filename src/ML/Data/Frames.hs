{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, TypeOperators, PartialTypeSignatures, AllowAmbiguousTypes, RankNTypes #-}

module ML.Data.Frames where

-- import GHC.Types (Symbol)
import qualified Data.Vinyl.Lens as V
import qualified Data.Vinyl.Core as V
import qualified Data.Vinyl.TypeLevel as V
import           Lens.Micro
import           Lens.Micro.Extras
-- import           Frames (Frame, Record)
import qualified Frames as F
import qualified Control.Foldl as L
import qualified Control.Foldl.Statistics as S

F.tableTypes "Adv" "/home/pi/wip/haskell/data-haskell/isl/data/Advertising.csv"

-- unapplied
-- normalizeColumn
--   :: (Functor f, Fractional b1) =>
--      Lens.Micro.Type.ASetter s b2 b1 b1 -> f s -> f b2
-- applied to a field, sales:
-- normalizeColumn  :: (Functor f,
--       vinyl-0.11.0:Data.Vinyl.Lens.RecElem
--         vinyl-0.11.0:Data.Vinyl.Core.Rec
--         Sales
--         Sales
--         rs
--         rs
--         (vinyl-0.11.0:Data.Vinyl.TypeLevel.RIndex Sales rs)) =>
--      f (Frames.Rec.Record rs) -> f (Frames.Rec.Record rs)
-- swapping the frame and the field, we get:
-- normalizeColumn advFrame
--   :: Fractional b1 =>
--      Lens.Micro.Type.ASetter ML.Data.FramesSuite.Adv b2 b1 b1
--      -> Frames.Frame.Frame b2
-- normalizeColumn :: (Functor f, V.RecElem V.Rec a a rs rs (V.RIndex a rs)) =>
normalizeColumn' :: a F.∈ rs =>
    (F.Record rs -> Double)
    -> ASetter (F.Record rs) (F.Record rs) Double Double
    -> F.FrameRec rs -> F.FrameRec rs
normalizeColumn' readF updateF frame =
    let mean = L.fold S.mean                           $ readF <$> frame
        vari = sqrt $ L.fold (S.varianceUnbiased mean) $ readF <$> frame
    in (updateF %~ ((/3) . subtract 4)) <$> frame


-- normalizeColumn :: DoubleField f a rs -> F.FrameRec rs -> F.FrameRec rs
normalizeColumn frame =
    let mean = L.fold S.mean                           $ view sales <$> frame
        vari = sqrt $ L.fold (S.varianceUnbiased mean) $ view sales <$> frame
    in (sales %~ ((/vari) . subtract mean)) <$> frame

type DoubleField f a rs = (Functor f, a F.∈ rs) =>
    (Double -> f Double) -> F.Record rs -> f (F.Record rs)
-- sales'' :: (Functor f, F.RecElem F.Rec Sales Sales rs rs (V.RIndex Sales rs)) =>
--      (Double -> f Double)
--      -> F.Record rs -> f (F.Record rs)
-- rewritten using type aliases
-- sales'' :: (Functor f, Sales F.∈ rs) =>
--     (Double -> f Double)
--      -> F.Record rs -> f (F.Record rs)
sales'' :: DoubleField f Sales rs
sales'' = sales
