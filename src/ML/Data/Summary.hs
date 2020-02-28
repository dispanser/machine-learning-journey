module ML.Data.Summary
    ( module ML.Data.Summary
    , (%), (%.)
    , F.sformat -- re-exporting formatting vocabular
    , F.fixed
    , F.stext
    , F.left
    ) where

import qualified Formatting as F
import qualified Formatting.ShortFormatters as F
import           Formatting ((%), (%.))
import           Data.Foldable (toList)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T

class Summary a where
  summary :: a -> [T.Text]

dSc :: Double -> Scientific.Scientific
dSc = Scientific.fromFloatDigits

scieF, percF :: F.Format r' (Scientific.Scientific -> r')
scieF = F.left 8 ' ' %. F.scifmt Scientific.Generic (Just 2)
percF = F.left 5 ' ' %. F.scifmt Scientific.Generic (Just 2)

textF :: Int -> F.Format r' (T.Text -> r')
textF i = (F.l i ' ' %. F.st)

floatF :: Real a => Int -> Int -> F.Format r' (a -> r')
floatF n d = (F.l n ' ' %. F.f d)

numF :: Real a => Int -> F.Format r' (a -> r')
numF w = F.l w ' ' %. (F.fitLeft w %. F.sf)

intF :: F.Format r (Integer -> r)
intF = F.r 4 ' ' %. F.d

formatSome :: Foldable t => Int -> T.Text -> t Double -> T.Text
formatSome w sep xs =
    let xs' = toList xs
        formattedItems = F.sformat (numF w) <$> xs'
    in T.intercalate sep formattedItems
