{-# LANGUAGE LambdaCase #-}

module Prelude
       ( module Relude
       , debugShow
       , zipAll
       ) where

import Relude
import qualified Debug.Trace as D

debugShow :: (Show a, Show b) => b -> a -> a
debugShow prefix v =
    let msg = show prefix <> " " <> show v
    in D.trace msg v

-- like zip, but for lists of lists: inverts the dimenions of the lists
zipAll :: Show a => [[a]] -> [[a]]
zipAll []           = []
zipAll yss =
    (sequence $ uncons <$> yss) & \case
         Just x -> let (row, rest) = unzip x
                   in  row : zipAll rest
         Nothing -> []

