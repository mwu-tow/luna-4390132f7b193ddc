{-# LANGUAGE OverloadedStrings #-}
module FuzzyText
    ( fuzzySearch
    , customSearch
    , RawEntry (..)
    , EntryType (..)
    , Match (..)
    , Scoring (..)
    , ImportInfo(..)
    , Range
    , Score
    , ClassName
    , ImportName
    , Query
    , name
    , doc
    , entry
    , exactMatch
    , score
    , charsMatch
    , weight
    , entryType
    , className
    , importInfo
    , importName
    , imported
    ) where

import           Data.Char
import qualified Data.List                    as List
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import qualified Data.Text                    as Text
import           Prologue
import           Control.Lens ((?~), Getter, to)



type Score      = Int
type Range      = (Int, Int)
type ClassName  = Text
type Query      = Text
type ImportName = Text

data EntryType 
    = Function 
    | Method ClassName 
    | Constructor ClassName 
    | Command deriving (Show, Eq)


data ImportInfo = ImportInfo
    { _importName :: ImportName
    , _imported   :: Bool
    } deriving (Eq, Show)

makeLenses ''ImportInfo

data RawEntry = RawEntry
    { _rName      :: Text
    , _rDoc       :: Text
    , _rEntryType :: EntryType
    , _rWeight    :: Double 
    , _rImport    :: Maybe ImportInfo
    } deriving (Show, Eq)

makeLenses ''RawEntry

data Scoring = Scoring
    { _mismatchPenalty :: Score
    , _skipPenalty     :: Score
    , _prefixBonus     :: Score
    , _sequenceBonus   :: Score
    , _suffixBonus     :: Score
    , _wordPrefixBonus :: Score
    , _wordSuffixBonus :: Score
    } deriving (Show, Eq)

makeLenses ''Scoring

data MatchState = MatchState
    { _query           :: Query
    , _msEntry         :: RawEntry                             
    , _scoring         :: Scoring
    , _positionInQuery :: Int
    , _positionInEntry :: Int
    , _charsScoring    :: IntMap Scoring
    , _matchedChars    :: [Range]
    } deriving (Show, Eq)

makeLenses ''MatchState

data Match = Match
    { _entry      :: RawEntry
    , _exactMatch :: Bool
    , _score      :: Score
    , _charsMatch :: [Range]
    } deriving (Show, Eq)

makeLenses ''Match

instance Default Scoring where 
    def = Scoring
        (-4) -- mismatchPenalty
        (-4) -- skipPenalty
        12   -- prefixBonus
        10   -- sequenceBonus
        4    -- suffixBonus
        6    -- wordPrefixBonus
        3    -- wordSuffixBonus

matchState :: Query -> (Maybe Scoring) -> RawEntry -> MatchState
matchState q mayS e = MatchState q e (fromJust def mayS) def def def def

class Entry a where
    name       :: Getter a Text
    doc        :: Getter a Text
    weight     :: Getter a Double
    entryType  :: Getter a EntryType
    importInfo :: Getter a (Maybe ImportInfo)
    className  :: Getter a Text
    className = to className' where
        className' e = case e ^. entryType of
            Function       -> def
            Method      cn -> cn
            Constructor cn -> cn
            Command        -> def

instance Entry RawEntry where
    name       = rName
    doc        = rDoc
    weight     = rWeight
    entryType  = rEntryType
    importInfo = rImport

instance Entry Match where
    name       = entry . name
    doc        = entry . doc
    weight     = entry . weight
    entryType  = entry . entryType
    importInfo = entry . importInfo

instance Entry MatchState where
    name       = msEntry . name
    doc        = msEntry . doc
    weight     = msEntry . weight
    entryType  = msEntry . entryType
    importInfo = msEntry . importInfo

instance Ord Match where
    m1 `compare` m2 = let 
        m1score = fromIntegral (m1 ^. score) * (m1 ^. weight)
        m2score = fromIntegral (m2 ^. score) * (m2 ^. weight)
        exactMatchOrd =
            if      m1 ^. exactMatch && not (m2 ^. exactMatch) then LT
            else if not (m1 ^. exactMatch) && m2 ^. exactMatch then GT
            else EQ            
        importOrd = case (,) <$> (m1 ^. importInfo) <*> (m2 ^. importInfo) of
            Nothing ->  EQ
            Just (mi1, mi2) ->
                if mi1 ^. imported && not (mi2 ^. imported) then LT
                else if not (mi1 ^. imported) && mi2 ^. imported then GT
                else EQ

        compareScore = 
            if m1 ^. score == 0 && m2 ^. score == 0
                then (m2 ^. weight) `compare` (m1 ^. weight)
            else if m1 ^. score < 0 && m2 ^. score < 0
                then (1/m2score) `compare` (1/m1score)
            else m2score `compare` m1score
        in 
            if      exactMatchOrd /= EQ then exactMatchOrd
            else if importOrd     /= EQ then importOrd
            else if compareScore  /= EQ then compareScore
            else (m1 ^. name) `compare` (m2 ^. name)

instance Convertible MatchState Match where
    convert ms = do
        let entry'      = ms ^. msEntry
            matchedCharsLength = foldl
                (\s (beg, end) -> s + end - beg) 
                def 
                (ms ^. matchedChars)
            exactMatch' = Text.length (ms ^. query) == matchedCharsLength 
            score'      = ms ^. currentScore 
            charMatch'  = reverse $ ms ^. matchedChars
        Match entry' exactMatch' score' charMatch'



fuzzySearch :: Query -> [RawEntry] -> [Match]
fuzzySearch q = List.sort . map (matchEntry q def)

customSearch :: Query -> [RawEntry] -> Scoring -> [Match]
customSearch q es s = List.sort $ map (matchEntry q (Just s)) es


matchEntry :: Query -> Maybe Scoring -> RawEntry -> Match
matchEntry = matchNext .:. matchState
    
matchNext :: MatchState -> Match
matchNext ms = maybe (convert ms) (uncurry processChars) $ mayCurrentChars where
    eci = ms ^. positionInEntry
    qci = ms ^. positionInQuery
    mayCurrentChars = (,) <$> lookupQueryChar qci ms <*> lookupEntryChar eci ms 
    processChars qc ec = if charactersMatch qc ec
        then min (matchNext $ updatedState qc ec) (skipChar ms)
        else skipChar ms
    updatedState qc ec = ms
        & positionInEntry       %~ succ
        & positionInQuery       %~ succ
        & charsScoring . at eci ?~ charScore qc ec
        & matchedChars          %~ updatedLettersMatch qc ec
    updatedLettersMatch qc ec r = if charactersMatch qc ec 
        then appendRange (toRange eci) r 
        else r
    charScore qc ec = Scoring
        (calculateMismatchPenalty qc ec ms)
        def
        (calculatePrefixBonus qc ec eci ms)
        (calculateSequenceBonus qc ec eci ms)
        (calculateSuffixBonus qc ec qci eci ms)
        (calculateWordPrefixBonus qc ec eci ms)
        (calculateWordSuffixBonus qc ec eci ms)

skipChar :: MatchState -> Match
skipChar = matchNext . updatedState where
    updatedState ms = ms 
        & positionInEntry %~ succ
        & charsScoring . at (ms ^. positionInEntry) ?~ 
            Scoring def (ms ^. scoring . skipPenalty) def def def def def


calculateMismatchPenalty :: Char -> Char -> MatchState -> Score
calculateMismatchPenalty qc ec ms = if qc == ec 
    then 0 
    else ms ^. scoring . mismatchPenalty

calculatePrefixBonus :: Char -> Char -> Int -> MatchState -> Score
calculatePrefixBonus qc ec eci ms = if qc /= ec
    then 0
    else case ms ^. matchedChars of
        []           -> if eci == 0 then ms ^. scoring . prefixBonus else 0
        [(beg, end)] -> if beg == 0 && end == eci 
            then (ms ^. scoring . prefixBonus) * (eci + 1) 
            else 0
        _            -> 0

calculateSequenceBonus :: Char -> Char -> Int -> MatchState -> Score
calculateSequenceBonus qc ec eci ms = if qc /= ec 
    then 0 
    else case ms ^. matchedChars of
        []           -> 0
        (beg, end):_ -> if end /= eci 
            then 0 
            else (eci - beg) * (ms ^. scoring . sequenceBonus)

    
calculateSuffixBonus :: Char -> Char -> Int -> Int -> MatchState -> Score
calculateSuffixBonus qc ec qci eci ms 
    = let isSuffix 
            = isJust (lookupQueryChar (succ qci) ms)
            || isJust (lookupEntryChar (succ eci) ms)
    in if qc /= ec || isSuffix
        then 0
        else case ms ^. matchedChars of
            []           -> ms ^. scoring . suffixBonus
            (beg, end):_ -> if end == eci
                then (end - beg + 1) * (ms ^. scoring . suffixBonus)
                else ms ^. scoring . suffixBonus


calculateWordPrefixBonus :: Char -> Char -> Int -> MatchState -> Score
calculateWordPrefixBonus qc ec eci ms = if qc /= ec 
    then 0 
    else prefixLength * (ms ^. scoring . wordPrefixBonus) where
        indexedMatch beg 
            = drop beg . zip [0..] . take (succ eci) . convert $ ms ^. name
        prefixLength = case ms ^. matchedChars of
            []           -> if isWordHead eci ec ms then 1 else 0
            (beg, end):_ -> if end /= eci && isWordHead eci ec ms then 1 
                else if end /= eci then 0
                else length . dropWhile (\(i, c) -> not $ isWordHead i c ms) 
                    $ indexedMatch beg


calculateWordSuffixBonus :: Char -> Char -> Int -> MatchState -> Score
calculateWordSuffixBonus qc ec eci ms
    = if qc /= ec || not (isWordLast eci ec ms)
        then 0 
        else case ms ^. matchedChars of
            [] -> ms ^. scoring . wordSuffixBonus
            (beg, end):_ -> if end == eci 
                then (end - beg + 1) * (ms ^. scoring . wordSuffixBonus) 
                else ms ^. scoring . wordSuffixBonus


charactersMatch :: Char -> Char -> Bool
charactersMatch c1 c2 = toLower c1 == toLower c2 

lookupEntryChar :: Int -> MatchState -> Maybe Char
lookupEntryChar i ms = ms ^. name ^? ix i

lookupQueryChar :: Int -> MatchState -> Maybe Char
lookupQueryChar i ms = ms ^. query ^? ix i


startsNewWord :: Char -> Char -> Bool
startsNewWord c prevC = let xor a b = (a && not b) || (not a && b)
    in (isLetter prevC `xor` isLetter c) || (isLower prevC && isUpper c)

isWordHead :: Int -> Char -> MatchState -> Bool
isWordHead ind c ms 
    = maybe True (startsNewWord c) $ lookupEntryChar (pred ind) ms

isWordLast :: Int -> Char -> MatchState -> Bool
isWordLast ind c ms 
    = maybe True (flip startsNewWord c) $ lookupEntryChar (succ ind) ms


currentScore :: Getter MatchState Int
currentScore = to currentScore' where
    currentScore' = sum . map toScore . IntMap.elems . view charsScoring
    toScore (Scoring b1 b2 b3 b4 b5 b6 b7) = b1 + b2 + b3 + b4 + b5 + b6 + b7

appendRange :: Range -> [Range] -> [Range]
appendRange i [] = [i]
appendRange i@(newBeg, newEnd) prev@((oldBeg, oldEnd):t) = if oldEnd == newBeg
    then (oldBeg, newEnd) : t
    else i : prev

toRange :: Int -> Range
toRange i = (i, succ i)



