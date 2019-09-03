module Prelude
       ( module Relude
       , debugShow
       ) where

import Relude
import qualified Debug.Trace as D

debugShow :: (Show a, Show b) => b -> a -> a
debugShow prefix v =
    let msg = show prefix <> " " <> show v
    in D.trace msg v
