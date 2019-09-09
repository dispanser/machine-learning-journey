{-# LANGUAGE LambdaCase #-}

module Prelude
       ( module Relude
       , debugShow
       , traceSome
       ) where

import Relude
import qualified Debug.Trace as D

debugShow :: (Show a, Show b) => b -> a -> a
debugShow prefix v =
    let msg = show prefix <> ": " <> show v
    in D.trace msg v

traceSome :: Show b => String -> (a -> b) -> a -> a
traceSome prefix f x =
    let msg = show prefix <> ": " <> show (f x)
    in D.trace msg x

