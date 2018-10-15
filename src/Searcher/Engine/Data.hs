module Searcher.Engine.Data where

import Searcher.Engine.Prelude

import qualified Data.Text                  as Text
import qualified Searcher.Engine.Data.Score as Score

import Searcher.Engine.Data.Range (Range)
import Searcher.Engine.Data.Score (Score)




class Eq a => SearcherData a where
    name              :: Getter a Name
    rawDocumentation  :: Getter a Documentation
    documentation     :: Getter a (Maybe Documentation)
    documentation = to $ \sd -> if Text.null $ sd ^. rawDocumentation
        then Nothing
        else Just $ sd ^. rawDocumentation
    prefix            :: Getter a Prefix
    score             :: Getter a Score
    hintTextSeparator :: Getter a Text
    prefixedName :: Getter a Name
    prefixedName = to $ \sd ->
        sd ^. prefix <> sd ^. hintTextSeparator <> sd ^. name

class IsMatch a where
    matchKind         :: Getter a MatchKind
    matchedCharacters :: Getter a [Range]


compareMatches :: (SearcherData a, IsMatch a) => a -> a -> Ordering
compareMatches m1 m2 =
    let notFullyMatchedOrd = if m1 ^. matchKind == m2 ^. matchKind then EQ
            else if m1 ^. matchKind == NotFullyMatched             then GT
            else if m2 ^. matchKind == NotFullyMatched             then LT
            else EQ
        matchTypeOrd = (m1 ^. matchKind) `compare` (m2 ^. matchKind)
        totalScore m = m ^. score . Score.total
        hintWeight m = m ^. score . Score.weight
        compareScore =
            if totalScore m1 == totalScore m2
                then (hintWeight m2) `compare` (hintWeight m1)
            else if totalScore m1 < 0 && totalScore m2 < 0
                then (1/totalScore m2) `compare` (1/totalScore m1)
                else totalScore m2 `compare` totalScore m1
    in
        if      notFullyMatchedOrd /= EQ then notFullyMatchedOrd
        else if matchTypeOrd       /= EQ then matchTypeOrd
        else if compareScore       /= EQ then compareScore
        else (m1 ^. name) `compare` (m2 ^. name)
