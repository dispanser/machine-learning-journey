module ML.Data.Summary
    ( module ML.Data.Summary
    , (%), (%.)
    , F.sformat -- re-exporting formatting vocabular
    ) where

import qualified Formatting as F
import qualified Formatting.ShortFormatters as F
import           Formatting ((%), (%.))
import qualified Data.Scientific as Scientific

class Summary a where
  summary :: a -> [Text]

dSc :: Double -> Scientific.Scientific
dSc = Scientific.fromFloatDigits

scieF, percF :: F.Format r' (Scientific.Scientific -> r')
scieF = F.left 11 ' ' %. F.scifmt Scientific.Generic (Just 2)
percF = F.left 5 ' ' %. F.scifmt Scientific.Generic (Just 2)

textF :: Int -> F.Format r' (Text -> r')
textF i = (F.l i ' ' %. F.st)

numF :: F.Format r' (Integer -> r')
numF  = F.l 13 ' ' %. (F.fitRight 13 %. F.sf)

intF :: F.Format r (Integer -> r)
intF = F.r 4 ' ' %. F.d

