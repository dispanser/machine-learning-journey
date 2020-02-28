module Visuals.Image where

import qualified Codec.Picture as JP
import           Codec.Picture.Types (Pixel8)
import           Data.Foldable (foldl')
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Word (Word8)

data PixelMap = PixelMap
    { pixels :: HashMap (Int, Int) Int
    , width  :: Int
    , height :: Int }

generateTimeSeriesImage :: (Double -> (Double, Double)) -> Int -> Int -> PixelMap
generateTimeSeriesImage f width' height' =
    let fx   = floor . (* fromIntegral width')
        fy   = floor . (* fromIntegral height')
        px t = let (x, y) = f t in (fx x, fy y)
    in PixelMap { pixels = foldl' (\m t -> HM.insert (px t) 1 m) HM.empty [0,0.0001 .. 1]
                , width  = width'
                , height = height' }

imageFromPixelMap :: PixelMap -> JP.Image Pixel8
imageFromPixelMap pxm =
    let px :: Int -> Int -> Word8
        px x y = fromIntegral $ 255 * HM.lookupDefault 0 (x, y) (pixels pxm)
    in JP.generateImage px (width pxm) (height pxm)
