{-# LANGUAGE OverloadedStrings #-}
module FuzzyText
    ( fuzzySearch
    , customSearch
    , RawEntry (..)
    , EntryType (..)
    , Match (..)
    , Scoring (..)
    , Range
    , Score
    , ClassName
    , Bonus
    , Query
    , name
    , entry
    , exactMatch
    , score
    , match
    , weight
    , entryType
    , className
    ) where

import           Data.Char
import qualified Data.List                    as List
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import qualified Data.Text                    as Text
import           Prologue



type Score     = Bonus
type Range     = (Int, Int)
type Bonus     = Int
type ClassName = Text
type Query     = Text

data EntryType = Function | Method ClassName | Constructor ClassName | Command deriving (Show, Eq)

data RawEntry = RawEntry { _rName       :: Text
                         , _rEntryType  :: EntryType
                         , _rWeight     :: Double 
                         } deriving (Show, Eq)
makeLenses ''RawEntry

data Scoring = Scoring { _perfectMatchBonus :: Bonus
                       , _mismatchPenalty :: Bonus
                       , _skipPenalty     :: Bonus
                       , _prefixBonus     :: Bonus
                       , _sequenceBonus   :: Bonus
                       , _suffixBonus     :: Bonus
                       , _wordPrefixBonus :: Bonus
                       , _wordSuffixBonus :: Bonus
                       } deriving (Show, Eq)
makeLenses ''Scoring

data MatchState = MatchState { _query           :: Query
                             , _msEntry         :: RawEntry                             
                             , _scoring         :: Scoring
                             , _positionInQuery :: Int
                             , _positionInEntry :: Int
                             , _charsScoring    :: IntMap Scoring
                             , _matched         :: [Range]
                             } deriving (Show, Eq)
makeLenses ''MatchState

data Match = Match { _entry      :: RawEntry
                   , _exactMatch :: Bool
                   , _score      :: Score
                   , _match      :: [Range]
                   } deriving (Show, Eq)
makeLenses ''Match

instance Default Scoring where def = Scoring 100  -- perfectMatchBonus
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
    name      :: Getter a Text
    weight    :: Getter a Double
    entryType :: Getter a EntryType
    className :: Getter a Text
    className = to className' where
        className' e = case e ^. entryType of
            Function       -> def
            Method      cn -> cn
            Constructor cn -> cn
            Command        -> def

instance Entry RawEntry where
    name      = rName
    weight    = rWeight
    entryType = rEntryType

instance Entry Match where
    name      = entry . name
    weight    = entry . weight
    entryType = entry . entryType

instance Entry MatchState where
    name      = msEntry . name
    weight    = msEntry . weight
    entryType = msEntry . entryType

instance Ord Match where
    m1 `compare` m2 = let 
        m1score = fromIntegral (m1 ^. score) * (m1 ^. weight)
        m2score = fromIntegral (m2 ^. score) * (m2 ^. weight)
        compareScore = if m1 ^. score < 0 && m2 ^. score < 0 then (1/m2score) `compare` (1/m1score) else m2score `compare` m1score
        in 
            if m1 ^. exactMatch && not (m2 ^. exactMatch) then LT
            else if not (m1 ^. exactMatch) && m2 ^. exactMatch then GT
            else if compareScore /= EQ then compareScore
            else (m1 ^. name) `compare` (m2 ^. name)


lookupEntryChar :: Int -> MatchState -> Maybe Char
lookupEntryChar i ms = ms ^. name ^? ix i

lookupQueryChar :: Int -> MatchState -> Maybe Char
lookupQueryChar i ms = ms ^. query ^? ix i

currentScore :: Getter MatchState Int
currentScore = to currentScore' where
    currentScore' = sum . map toScore . IntMap.elems . view charsScoring
    toScore (Scoring b1 b2 b3 b4 b5 b6 b7 b8) = b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8

appendRange :: Range -> [Range] -> [Range]
appendRange i [] = [i]
appendRange i@(newBeg, newEnd) prev@((oldBeg, oldEnd):t) = if oldEnd == newBeg
    then (oldBeg, newEnd) : t
    else i : prev


fuzzySearch :: Query -> [RawEntry] -> [Match]
fuzzySearch q = List.sort . map (matchEntry q def)

customSearch :: Query -> [RawEntry] -> Scoring -> [Match]
customSearch q es s = List.sort $ map (matchEntry q (Just s))  es

instance Convertible MatchState Match where
    convert ms = do
        let entry'      = ms ^. msEntry
            exactMatch' = Text.length (ms ^. query) == foldl (\s (beg, end) -> s + end - beg) def (ms ^. matched)
            score'      = ms ^. currentScore 
            match'      = reverse $ ms ^. matched
        Match entry' exactMatch' score' match'   

toRange :: Int -> Range
toRange i = (i, succ i)

matchEntry :: Query -> Maybe Scoring -> RawEntry -> Match
matchEntry = matchNext .:. matchState

matches :: Char -> Char -> Bool
matches c1 c2 = toLower c1 == toLower c2 

matchNext :: MatchState -> Match
matchNext ms = do
    let eci = ms ^. positionInEntry
        qci = ms ^. positionInQuery
        mayChars = (,) <$> lookupEntryChar eci ms <*> lookupQueryChar qci ms
        match' ec qc = if matches ec qc then min (matchChars ms) (skipChar ms) else skipChar ms
    maybe (convert ms) (uncurry match') mayChars 

matchChars :: MatchState -> Match
matchChars ms = maybe (convert ms) (matchNext . uncurry updatedState) $ mayCurrentChars where
    eci = ms ^. positionInEntry
    qci = ms ^. positionInQuery
    mayCurrentChars = (,) <$> lookupQueryChar qci ms <*> lookupEntryChar eci ms 
    updatedState qc ec = ms & positionInEntry %~ succ
                            & positionInQuery %~ succ
                            & charsScoring . at eci ?~ score' qc ec
                            & matched %~ match' qc ec
    match' qc ec r = if qc `matches` ec then appendRange (toRange eci) r else r
    score' qc ec = Scoring perfectMatchBonus' (mismatchPenalty' qc ec) skipPenalty' (prefixBonus' qc ec) (sequenceBonus' qc ec) (suffixBonus' qc ec) (wordPrefixBonus' qc ec) (wordSuffixBonus' qc ec)
    perfectMatchBonus' = if isNothing (lookupQueryChar (succ qci) ms)
                         && isNothing (lookupEntryChar (succ eci) ms)
                         && ms ^. name == ms ^. query then ms ^. scoring . perfectMatchBonus else 0
    mismatchPenalty' qc ec = if qc == ec then 0 else ms ^. scoring . mismatchPenalty
    skipPenalty' = def
    prefixBonus' qc ec = if qc /= ec then 0 else case ms ^. matched of
        []           -> if eci == 0 then ms ^. scoring . prefixBonus else 0
        [(beg, end)] -> if beg == 0 && end == eci then (ms ^. scoring . prefixBonus) * (eci + 1) else 0
        _            -> 0
    sequenceBonus' qc ec = if qc /= ec then 0 else case ms ^. matched of
        []           -> 0
        (beg, end):_ -> if end /= eci then 0 else (eci - beg) * (ms ^. scoring . sequenceBonus)
    suffixBonus' qc ec = if qc /= ec || isJust (lookupQueryChar (succ qci) ms) || isJust (lookupEntryChar (succ eci) ms) then 0
        else case ms ^. matched of
            []           -> ms ^. scoring . suffixBonus
            (beg, end):_ -> if end == eci then (end - beg + 1) * (ms ^. scoring . suffixBonus) else ms ^. scoring . suffixBonus
    xor a b = (a && not b) || (not a && b)
    isWordHead c prevC = (isLetter c `xor` isLetter prevC) || (isLower prevC && isUpper c)
    startsNewWord i c = maybe True (isWordHead c) $ lookupEntryChar (pred i) ms
    endsWord      i c = maybe True (flip isWordHead c) $ lookupEntryChar (succ i) ms
    wordPrefixBonus' qc ec = if qc /= ec then 0 else do
        let prefixLength = case ms ^. matched of
                []           -> if startsNewWord eci ec then 1 else 0
                (beg, end):_ -> let indexedMatch = drop beg . zip [0..] . take (succ eci) . convert $ ms ^. name in
                    if end /= eci && startsNewWord eci ec then 1 
                    else if end /= eci then 0
                    else length $ dropWhile (not . uncurry startsNewWord) indexedMatch
        prefixLength * (ms ^. scoring . wordPrefixBonus)
    wordSuffixBonus' qc ec = if qc /= ec || not (endsWord eci ec) then 0 else case ms ^. matched of
        [] -> ms ^. scoring . wordSuffixBonus
        (beg, end):_ -> if end == eci then (end - beg + 1) * (ms ^. scoring . wordSuffixBonus) else ms ^. scoring . wordSuffixBonus



skipChar :: MatchState -> Match
skipChar ms = matchNext updatedState where
    updatedState = ms & positionInEntry %~ succ
                      & charsScoring . at (ms ^. positionInEntry) ?~ Scoring def def (ms ^. scoring . skipPenalty) def def def def def

-- calculateBonus q (c, ind) entryMaxInd wordBeg isWordSuffix (_, inds) bonuses = sum activeBonuses where
--     activeBonuses = [ mismatchPenalty' 
--                     , omittedLettersPenalty'
--                     , prefixBonus'
--                     , sequenceBonus'
--                     , suffixBonus'
--                     , wordPrefixBonus'
--                     ]
--     getBonus :: Text -> (Int -> Int) -> Int
--     getBonus bonusName calc = maybe 0 calc $ Map.lookup bonusName bonuses
--     wordPrefixBonus'       = getBonus "wordPrefixBonus"       $ \b -> let (beg, end) = List.head inds in
--         if null inds || ind == 0 then b
--         else if not (null inds) && end + 1 == ind && beg <= lastCapIndex then (ind - lastCapIndex + 1) * b
--         else 0
--     sequenceBonus'         = getBonus "sequenceBonus"         $ \b -> let (beg, end) = List.head inds in 
--         if null inds || end + 1 /= ind then 0 else (ind - beg) * b
--     prefixBonus'   = getBonus "prefixBonus"   $ \b -> let (beg, end) = List.head inds in
--         if mismatchPenalty' /= 0                              then 0
--         else if null inds && ind == 0                         then b
--         else if not (null inds) && beg == 0 && end + 1 == ind then (ind + 1) * b
--         else 0
--     suffixBonus'           = getBonus "suffixBonus"           $ \b -> if Text.length q == 1 && entryMaxInd == ind then b else 0
--     omittedLettersPenalty' = getBonus "omittedLettersPenalty" $ \p -> let end = snd $ List.head inds in
--         if null inds then ind * p else (ind - end - 1) * p
--     mismatchPenalty'        = getBonus "mismatchPenalty"      $ \p -> if Text.null q || Text.head q == c then 0 else p
--     wordSuffixBonus         = getBonus "wordSuffix"           $ \p -> if Text.null q then 0 else 0



-- matchEntry :: Query -> RawEntry -> Match
-- matchEntry q re = max (matchChars es) (skipChar es) where
--     es = toMatchState q re


-- processEntry :: Query -> [(Char, Int)] -> (Score, [Range]) -> Int -> (Score, [Range])
-- processEntry q e prevRes@(prevScore, prevMatch) wordBeg = if Text.null q then (prevScore + 1, prevMatch) else if null e then prevRes else bestMatch e wordBeg where
--     bestMatch :: [(Char, Int)] -> Int -> (Score, [Range])
--     bestMatch e' wordBeg = if null e' then prevRes else do
--         let (c, i)                    = List.head e'
--             newLastCapIndex           = if isUpper c then i else lastCapIndex
--         if not $ firstLettersMatch q e' then bestMatch (List.tail e') newLastCapIndex else do
--             let useFirstLetterResult      = processEntry (Text.tail q) (List.tail e') (getScore q e' prevRes newLastCapIndex bonusMap, appendRange (i, i) prevMatch) newLastCapIndex
--                 doNotUseFirstLetterResult = bestMatch (List.tail e') newLastCapIndex
--             if fst useFirstLetterResult >= fst doNotUseFirstLetterResult 
--                 then useFirstLetterResult
--                 else doNotUseFirstLetterResult

-- firstLettersMatch :: Query -> [(Char, Int)] -> Bool
-- firstLettersMatch q t = toLower (Text.head q) == toLower (fst $ List.head t)


-- getScore :: Query -> [(Char, Int)] -> (Score, [Range]) -> Int -> Map Text Bonus -> Score
-- getScore q t (s, inds) lastCapIndex bonuses = if Text.null q then s + 1
--     else if null t || not (firstLettersMatch q t)     then s
--     else s + 1 + (calculateBonus q (List.head t) (snd $ List.last t) lastCapIndex (s, inds) bonuses)

-- calculateBonus :: Query -> [(Char, Int)] -> Int -> Int -> Bool -> (Score, [Range]) -> Map Text Bonus -> Bonus
-- calculateBonus q (c, ind) entryMaxInd wordBeg isWordSuffix (_, inds) bonuses = sum activeBonuses where
--     activeBonuses = [ mismatchPenalty' 
--                     , omittedLettersPenalty'
--                     , prefixBonus'
--                     , sequenceBonus'
--                     , suffixBonus'
--                     , wordPrefixBonus'
--                     ]
--     getBonus :: Text -> (Int -> Int) -> Int
--     getBonus bonusName calc = maybe 0 calc $ Map.lookup bonusName bonuses
--     wordPrefixBonus'       = getBonus "wordPrefixBonus"       $ \b -> let (beg, end) = List.head inds in
--         if null inds || ind == 0 then b
--         else if not (null inds) && end + 1 == ind && beg <= lastCapIndex then (ind - lastCapIndex + 1) * b
--         else 0
--     sequenceBonus'         = getBonus "sequenceBonus"         $ \b -> let (beg, end) = List.head inds in 
--         if null inds || end + 1 /= ind then 0 else (ind - beg) * b
--     prefixBonus'   = getBonus "prefixBonus"   $ \b -> let (beg, end) = List.head inds in
--         if mismatchPenalty' /= 0                              then 0
--         else if null inds && ind == 0                         then b
--         else if not (null inds) && beg == 0 && end + 1 == ind then (ind + 1) * b
--         else 0
--     suffixBonus'           = getBonus "suffixBonus"           $ \b -> if Text.length q == 1 && entryMaxInd == ind then b else 0
--     omittedLettersPenalty' = getBonus "omittedLettersPenalty" $ \p -> let end = snd $ List.head inds in
--         if null inds then ind * p else (ind - end - 1) * p
--     mismatchPenalty'        = getBonus "mismatchPenalty"      $ \p -> if Text.null q || Text.head q == c then 0 else p
--     wordSuffixBonus         = getBonus "wordSuffix"           $ \p -> if Text.null q then 0 else 0


-- -- reverse omittedLettersPenalty on suffix and word suffix (in this case maybe part of it)

-- -- Jak jest pust to cofamy penalty * waga, jak nie jest to cofamy penalty * waga dla tego slowa
