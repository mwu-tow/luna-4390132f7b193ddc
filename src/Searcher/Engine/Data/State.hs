module Searcher.Engine.Data.State
    ( module Searcher.Engine.Data.State
    , module X
    ) where

import Searcher.Engine.Data       as X
import Searcher.Engine.Data.Range as X (Range)
import Searcher.Engine.Data.Score as X (Scoring (Scoring))
import Searcher.Engine.Prelude    as X (Query)

import Searcher.Engine.Prelude

import qualified Data.IntMap                as IntMap
import qualified Data.Text                  as Text
import qualified Searcher.Engine.Data       as Data
import qualified Searcher.Engine.Data.Score as Score

import Data.Char                  (isLetter, isLower, isUpper, toLower)
import Data.IntMap                (IntMap)
import Searcher.Engine.Data       (IsMatch, SearcherData, compareMatches)
import Searcher.Engine.Data.Range (appendRange, characterRange)


data State a = State
    { _hint              :: a
    , _query             :: Query
    , _positionInQuery   :: Int
    , _positionInData    :: Int
    , _scoring           :: Scoring
    , _charactersScoring :: IntMap Scoring
    , _matched           :: [Range]
    } deriving (Show, Eq)

makeLenses ''State

instance SearcherData a => SearcherData (State a) where
    name              = hint . Data.name
    rawDocumentation  = hint . Data.rawDocumentation
    prefix            = hint . Data.prefix
    hintTextSeparator = hint . Data.hintTextSeparator
    score =
        let points = sum . map toPoints . IntMap.elems . view charactersScoring
            toPoints (Scoring b1 b2 b3 b4 b5 b6 b7)
                = b1 + b2 + b3 + b4 + b5 + b6 + b7
            score' state
                = (state ^. hint . Data.score) & Score.points .~ points state
        in to score'

instance SearcherData a => IsMatch (State a) where
    matchedCharacters = matched
    matchKind         = to $ \state ->
        let query' = state ^. query
            name'   = state ^. hint . Data.name
            matchedCharsLength = foldl
                (\s (beg, end) -> s + end - beg)
                def
                (state ^. matched)
        in if query' == name'
                then CaseSensitiveEquality
            else if Text.toLower query' == Text.toLower name'
                then CaseInsensitiveEquality
            else if Text.length query' == matchedCharsLength
                then AllCharsMatched
                else NotFullyMatched

instance SearcherData a => Ord (State a) where
    compare = compareMatches



mkState :: SearcherData a => a -> Query -> (Maybe Scoring) -> State a
mkState data' query' (fromJust def -> scoring')
    = State data' query' def def scoring' def def

matchNextCharacter :: SearcherData a => State a -> State a
matchNextCharacter state = matchIfNotFinished where
    matchIfNotFinished = maybe state (uncurry processChars) mayCurrentChars
    dci = state ^. positionInData
    qci = state ^. positionInQuery
    mayCurrentChars = (,)
        <$> lookupQueryChar qci state
        <*> lookupDataChar dci state
    charactersMatch c1 c2 = toLower c1 == toLower c2
    processChars qc dc = if charactersMatch qc dc
        then min (matchNextCharacter $ updatedState qc dc) (skipChar state)
        else skipChar state
    updatedState qc dc = state
        & positionInData             %~ succ
        & positionInQuery            %~ succ
        & charactersScoring . at dci ?~ charScore qc dc
        & matched                    %~ updatedLettersMatch qc dc
    updatedLettersMatch qc dc r = if charactersMatch qc dc
        then appendRange (characterRange dci) r
        else r
    charScore qc dc = Scoring
        (calculateMismatchPenalty qc dc state)
        def
        (calculatePrefixBonus     qc dc dci state)
        (calculateSequenceBonus   qc dc dci state)
        (calculateSuffixBonus     qc dc qci dci state)
        (calculateWordPrefixBonus qc dc dci state)
        (calculateWordSuffixBonus qc dc dci state)

skipChar :: SearcherData a => State a -> State a
skipChar = matchNextCharacter . updatedState where
    updatedState state = state
        & positionInData                                   %~ succ
        & charactersScoring . at (state ^. positionInData) ?~ Scoring
            def
            (state ^. scoring . Score.skipPenalty)
            def
            def
            def
            def
            def

calculateMismatchPenalty :: SearcherData a => Char -> Char -> State a -> Int
calculateMismatchPenalty qc dc state = if qc == dc
    then 0
    else state ^. scoring . Score.mismatchPenalty

calculatePrefixBonus :: SearcherData a => Char -> Char -> Int -> State a -> Int
calculatePrefixBonus qc dc dci state = bonus * multiplier where
    bonus = if qc /= dc then 0 else state ^. scoring . Score.prefixBonus
    multiplier = case state ^. matched of
        []           -> if dci == 0               then 1       else 0
        [(beg, end)] -> if beg == 0 && end == dci then dci + 1 else 0
        _            -> 0

calculateSequenceBonus
    :: SearcherData a => Char -> Char -> Int -> State a -> Int
calculateSequenceBonus qc dc dci state = bonus * multiplier where
    bonus = if qc /= dc then 0 else state ^. scoring . Score.sequenceBonus
    multiplier = case state ^. matched of
        []           -> 0
        (beg, end):_ -> if end /= dci then 0 else dci - beg


calculateSuffixBonus
    :: SearcherData a => Char -> Char -> Int -> Int -> State a -> Int
calculateSuffixBonus qc dc qci dci state = bonus * multiplier where
    isSuffix
        =  isJust (lookupQueryChar (succ qci) state)
        || isJust (lookupDataChar  (succ dci) state)
    bonus = if qc /= dc || isSuffix
        then 0
        else state ^. scoring . Score.suffixBonus
    multiplier = case state ^. matched of
        []           -> 1
        (beg, end):_ -> if end == dci then end - beg + 1 else 1


calculateWordPrefixBonus
    :: SearcherData a => Char -> Char -> Int -> State a -> Int
calculateWordPrefixBonus qc dc dci state = bonus * multiplier where
    indexedData = zip [0..] . convert $ state ^. hint . Data.name
    bonus = if qc /= dc then 0 else state ^. scoring . Score.wordPrefixBonus
    multiplier = case state ^. matched of
        []           -> if isWordHead dci dc state then 1 else 0
        (beg, end):_ ->
            if      end /= dci && isWordHead dci dc state then 1
            else if end /= dci                            then 0
            else length $ dropWhile
                (\(i, c) -> not $ isWordHead i c state)
                (drop beg . take (succ dci) $ indexedData)

calculateWordSuffixBonus
    :: SearcherData a => Char -> Char -> Int -> State a -> Int
calculateWordSuffixBonus qc dc dci state = bonus * multiplier where
    bonus = if qc /= dc || not (isWordLast dci dc state)
        then 0
        else state ^. scoring . Score.wordSuffixBonus
    multiplier = case state ^. matched of
        []           -> 1
        (beg, end):_ -> if end == dci then end - beg + 1 else 1


lookupDataChar :: SearcherData a => Int -> State a -> Maybe Char
lookupDataChar i state = state ^. hint . Data.name ^? ix i

lookupQueryChar :: SearcherData a => Int -> State a -> Maybe Char
lookupQueryChar i state = state ^. query ^? ix i


startsNewWord :: Char -> Char -> Bool
startsNewWord c prevC = let xor a b = (a && not b) || (not a && b)
    in (isLetter prevC `xor` isLetter c) || (isLower prevC && isUpper c)

isWordHead :: SearcherData a => Int -> Char -> State a -> Bool
isWordHead ind c = maybe True (startsNewWord c) . lookupDataChar (pred ind)

isWordLast :: SearcherData a => Int -> Char -> State a -> Bool
isWordLast ind c = maybe True (flip startsNewWord c) . lookupDataChar (succ ind)

