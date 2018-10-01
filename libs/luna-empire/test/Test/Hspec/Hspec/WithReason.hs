--[TODO]: This functionality is super-useful with any Hspec work.
--        It should be exported to outter library or even to Test.Hspec as PR.
module Test.Hspec.Hspec.WithReason where

import Empire.Prelude

import Test.Hspec (Arg, Example, SpecWith, before_, it, pendingWith)


xitWithReason :: (HasCallStack, Example a)
    => String -> String -> a -> SpecWith (Arg a)
xitWithReason label reason action
    = before_ (pendingWith reason) $ it label action
