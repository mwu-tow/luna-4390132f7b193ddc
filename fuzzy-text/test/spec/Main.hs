module Main where

import Prelude

import qualified Spec

import Test.Hspec

main :: IO ()
main = hspec Spec.spec

