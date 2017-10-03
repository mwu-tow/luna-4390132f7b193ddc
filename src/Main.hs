{-# LANGUAGE OverloadedStrings #-}
module Main where

import FuzzyText
import Prologue
import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Map                     as Map
import           LunaStudio.Data.NodeSearcher (ModuleHints (ModuleHints))

main :: IO ()
main = do
    let res = processEntries (Text.pack "x") $ toEntries (Map.singleton "SomeModule" $ ModuleHints ["xx"] def) True
    pprint $ List.sort res
