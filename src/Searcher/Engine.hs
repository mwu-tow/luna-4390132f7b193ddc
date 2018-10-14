module Searcher.Engine
    ( module Searcher.Engine
    , module X
    ) where

import Searcher.Engine.Data         as X
import Searcher.Engine.Data.Command as X (Command (Command))
import Searcher.Engine.Data.Match   as X (Match (Match))
import Searcher.Engine.Data.Range   as X (Range)
import Searcher.Engine.Data.Score   as X (Fixed, Score (Score),
                                          Scoring (Scoring), Weight)
import Searcher.Engine.Data.Symbol  as X (Library (Library), Symbol (Symbol))
import Searcher.Engine.Prelude      as X (Documentation, MatchKind (AllCharsMatched, CaseInsensitiveEquality, CaseSensitiveEquality, NotFullyMatched),
                                          Name, Prefix, Query)

import Searcher.Engine.Prelude

import qualified Searcher.Engine.Data.Match as Match


search :: SearcherData a => Query -> [a] -> [Match a]
search q = sort . map (\d -> Match.matchSearcherData d q def)

customSearch :: SearcherData a => Query -> [a] -> Scoring -> [Match a]
customSearch q es s = sort $ map (\d -> Match.matchSearcherData d q (Just s)) es
