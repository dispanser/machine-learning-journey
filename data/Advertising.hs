-- minimal file to get template haskell splices into ghci
{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}

module Advertising where

import qualified Frames as F

advertisingFilepath :: FilePath
advertisingFilepath = "/home/pi/wip/haskell/data-haskell/isl/data/Advertising.csv"

-- advertisingFilepath can't be used here due to GHC stage restrictions
F.tableTypes "Adv" "/home/pi/wip/haskell/data-haskell/isl/data/Advertising.csv"

