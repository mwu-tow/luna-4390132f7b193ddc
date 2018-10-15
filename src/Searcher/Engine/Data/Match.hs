module Searcher.Engine.Data.Match
    ( module Searcher.Engine.Data.Match
    , module X
    ) where

import Searcher.Engine.Data       as X
import Searcher.Engine.Data.Range as X (Range)
import Searcher.Engine.Prelude    as X (MatchKind)

import Searcher.Engine.Prelude

import qualified Data.Text                  as Text
import qualified Searcher.Engine.Data       as Data
import qualified Searcher.Engine.Data.Score as Score
import qualified Searcher.Engine.Data.State as State

import Searcher.Engine.Data.Score (Scoring)
import Searcher.Engine.Data.State (State, matchNextCharacter, mkState)


data Match a = Match
    { _hint    :: a
    , _kind    :: MatchKind
    , _points  :: Int
    , _matched :: [Range]
    } deriving (Eq, Show)

makeLenses ''Match


instance SearcherData a => SearcherData (Match a) where
    name              = hint . Data.name
    rawDocumentation  = hint . Data.rawDocumentation
    prefix            = hint . Data.prefix
    hintTextSeparator = hint . Data.hintTextSeparator
    score =
        let hintScore m = m ^. hint . Data.score
            score'    m = hintScore m & Score.points %~ (+ m ^. points)
        in to score'

instance IsMatch (Match a) where
    matchKind         = kind
    matchedCharacters = matched

instance SearcherData a => Ord (Match a) where
    compare = compareMatches

instance SearcherData a => Convertible (State a) (Match a) where
    convert state =
        let hint' = state ^. State.hint
            eqCaseInsensitive t1 t2 = Text.toLower t1 == Text.toLower t2
            matchedCharsLength = foldl
                (\s (beg, end) -> s + end - beg)
                def
                (state ^. State.matched)
            kind' =
                if state ^. State.query == hint' ^. name
                    then CaseSensitiveEquality
                else if eqCaseInsensitive (state ^. State.query) (hint' ^. name)
                    then CaseInsensitiveEquality
                else if Text.length (state ^. State.query) == matchedCharsLength
                    then AllCharsMatched
                    else NotFullyMatched
            points' = state ^. Data.score . Score.points
            matchedChatacters = reverse $ state ^. State.matched
        in Match hint' kind' points' matchedChatacters


matchSearcherData :: SearcherData a => a -> Query -> Maybe Scoring -> Match a
matchSearcherData = convert . matchNextCharacter .:. mkState
