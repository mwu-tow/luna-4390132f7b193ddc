module NodeEditor.React.Model.Searcher.Hint
    ( module NodeEditor.React.Model.Searcher.Hint
    , module X
    ) where

import Searcher.Engine            as X (Command, Match, MatchKind (AllCharsMatched, CaseInsensitiveEquality, CaseSensitiveEquality, NotFullyMatched),
                                        Name, Prefix, Range, Symbol)
import Searcher.Engine.Data.Match as X (IsMatch (matchKind, matchedCharacters), SearcherData (documentation, hintTextSeparator, name, prefix, prefixedName, rawDocumentation, score))

import Common.Prelude


data Hint
    = CommandHint (Match Command)
    | NodeHint    (Match Symbol)
    deriving (Eq, Generic, Show)

makePrisms ''Hint


instance SearcherData Hint where
    name = lens getter setter where
        getter (CommandHint h) = h ^. name
        getter (NodeHint    h) = h ^. name
        setter (CommandHint h) n = CommandHint $ h & name .~ n
        setter (NodeHint    h) n = NodeHint    $ h & name .~ n
    rawDocumentation = lens getter setter where
        getter (CommandHint h) = h ^. rawDocumentation
        getter (NodeHint    h) = h ^. rawDocumentation
        setter (CommandHint h) d = CommandHint $ h & rawDocumentation .~ d
        setter (NodeHint    h) d = NodeHint    $ h & rawDocumentation .~ d
    prefix = to getter where
        getter (CommandHint h) = h ^. prefix
        getter (NodeHint    h) = h ^. prefix
    score = to getter where
        getter (CommandHint h) = h ^. score
        getter (NodeHint    h) = h ^. score
    hintTextSeparator = to getter where
        getter (CommandHint h) = h ^. hintTextSeparator
        getter (NodeHint    h) = h ^. hintTextSeparator

instance IsMatch Hint where
    matchKind = to getter where
        getter (CommandHint h) = h ^. matchKind
        getter (NodeHint    h) = h ^. matchKind
    matchedCharacters = to getter where
        getter (CommandHint h) = h ^. matchedCharacters
        getter (NodeHint    h) = h ^. matchedCharacters
