module Hive.Tools where

import Hive.Hid

import qualified Data.Map as Map
import qualified Data.Set as Set


cellsBorders :: Map.Map CID a -> Set.Set LID
cellsBorders cMap = Set.unions [Set.fromList (map f (Map.keys cMap)) | f <- c2l]

cellsCorners :: Map.Map CID a -> Set.Set NID
cellsCorners cMap = Set.unions [Set.fromList (map f (Map.keys cMap)) | f <- c2n]