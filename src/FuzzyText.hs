{-# LANGUAGE OverloadedStrings #-}
module FuzzyText
    ( fuzzySearch
    , Entry (..)
    , EntryType (..)
    , Score
    , Indices
    , ClassName
    , Query
    , name
    , entryType
    , weight
    , score
    , match
    , exactMatch
    , className
    ) where

import           Data.Char
import qualified Data.List                    as List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Text                    as Text
import           Prologue


type Score     = Int
type Indices   = (Int, Int)
type Bonus     = Int
type ClassName = Text
type Query     = Text

data EntryType = Function | Method ClassName | Constructor ClassName | Command deriving (Show, Eq)

data Entry = Entry { _name       :: Text
                   , _entryType  :: EntryType
                   , _weight     :: Double 
                   , _score      :: Score
                   , _match      :: [Indices]
                   , _exactMatch :: Bool
                   } deriving (Show, Eq)

makeLenses ''Entry

className :: Getter Entry Text
className = to className' where
    className' e = case e ^. entryType of
        Function       -> def
        Method      cn -> cn
        Constructor cn -> cn
        Command        -> def

instance Ord Entry where
    e1 `compare` e2 = let compareScore = (fromIntegral (e2 ^. score) * (e2 ^. weight)) `compare` (fromIntegral (e1 ^. score) * (e1 ^. weight)) in
        if e1 ^. exactMatch && not (e2 ^. exactMatch) then LT
        else if not (e1 ^. exactMatch) && e2 ^. exactMatch then GT
        else if compareScore /= EQ then compareScore
        else (e1 ^. name) `compare` (e2 ^. name)

bonusMap :: Map Text Bonus
bonusMap = Map.fromList [wordPrefixBonus, prefixSequenceBonus, sequenceBonus, suffixBonus, omittedLettersPenalty, mismatchPenalty]

wordPrefixBonus, prefixSequenceBonus, sequenceBonus, suffixBonus, omittedLettersPenalty, mismatchPenalty :: (Text, Bonus)
wordPrefixBonus       = ("wordPrefixBonus",       6)
sequenceBonus         = ("sequenceBonus",         10)
prefixSequenceBonus   = ("prefixSequenceBonus",   12)
suffixBonus           = ("suffixBonus",           4)
omittedLettersPenalty = ("omittedLettersPenalty", -1)
mismatchPenalty       = ("mismatchPenalty",       -5)

fuzzySearch :: Query -> [Entry] -> [Entry]
fuzzySearch q e = List.sort $ map updateEntry e where
    updateEntry e' = do
        let (sc, m) = getScoreAndMatch (e' ^. name)
            matchLength = foldl (\s (beg, end) -> s + end - beg + 1) 0 m
        e' & score      .~ sc
           & match      .~ m
           & exactMatch .~ if matchLength == Text.length q then True else False
    getScoreAndMatch n = (processEntry q (zip (Text.unpack n) [0..]) def (-1)) & _2 %~ List.reverse

processEntry :: Query -> [(Char, Int)] -> (Score, [Indices]) -> Int -> (Score, [Indices])
processEntry q e prevRes@(prevScore, prevMatch) lastCapitalIndex = if Text.null q then (prevScore + 1, prevMatch) else if null e then prevRes else bestMatch e lastCapitalIndex where
    bestMatch :: [(Char, Int)] -> Int -> (Score, [Indices])
    bestMatch e' lastCapIndex = if null e' then prevRes else do
        let (c, i)                    = List.head e'
            newLastCapIndex           = if isUpper c then i else lastCapIndex
        if not $ firstLettersMatch q e' then bestMatch (List.tail e') newLastCapIndex else do
            let useFirstLetterResult      = processEntry (Text.tail q) (List.tail e') (getScore q e' prevRes newLastCapIndex bonusMap, appendIndices (i, i) prevMatch) newLastCapIndex
                doNotUseFirstLetterResult = bestMatch (List.tail e') newLastCapIndex
            if fst useFirstLetterResult >= fst doNotUseFirstLetterResult 
                then useFirstLetterResult
                else doNotUseFirstLetterResult

firstLettersMatch :: Query -> [(Char, Int)] -> Bool
firstLettersMatch q t = toLower (Text.head q) == toLower (fst $ List.head t)

appendIndices :: Indices -> [Indices] -> [Indices]
appendIndices i []    = [i]
appendIndices i@(newBeg, newEnd) prev@((oldBeg, oldEnd):t) = if oldEnd + 1 == newBeg
    then (oldBeg, newEnd) : t
    else i : prev

getScore :: Query -> [(Char, Int)] -> (Score, [Indices]) -> Int -> Map Text Bonus -> Score
getScore q t (s, inds) lastCapIndex bonuses = if Text.null q then s + 1
    else if null t || not (firstLettersMatch q t)     then s
    else s + 1 + (calculateBonus q (List.head t) (snd $ List.last t) lastCapIndex (s, inds) bonuses)

calculateBonus :: Query -> (Char, Int) -> Int -> Int -> (Score, [Indices]) -> Map Text Bonus -> Bonus
calculateBonus q (c, ind) entryMaxInd lastCapIndex (_, inds) bonuses = prefixSequenceBonus' + wordPrefixBonus' + sequenceBonus' + suffixBonus' + omittedLettersPenalty' + mismatchPenalty' where
    getBonus :: Text -> (Int -> Int) -> Int
    getBonus bonusName calc = maybe 0 calc $ Map.lookup bonusName bonuses
    wordPrefixBonus'       = getBonus "wordPrefixBonus"       $ \b -> let (beg, end) = List.head inds in
        if null inds || ind == 0 then b
        else if not (null inds) && end + 1 == ind && beg <= lastCapIndex then (ind - lastCapIndex + 1) * b
        else 0
    sequenceBonus'         = getBonus "sequenceBonus"         $ \b -> let (beg, end) = List.head inds in 
        if null inds || end + 1 /= ind then 0 else (ind - beg) * b
    prefixSequenceBonus'   = getBonus "prefixSequenceBonus"   $ \b -> let (beg, end) = List.head inds in
        if mismatchPenalty' /= 0                              then 0
        else if null inds && ind == 0                         then b
        else if not (null inds) && beg == 0 && end + 1 == ind then (ind + 1) * b
        else 0
    suffixBonus'           = getBonus "suffixBonus"           $ \b -> if Text.length q == 1 && entryMaxInd == ind then b else 0
    omittedLettersPenalty' = getBonus "omittedLettersPenalty" $ \p -> let end = snd $ List.head inds in
        if null inds then ind * p else (ind - end - 1) * p
    mismatchPenalty' = getBonus "mismatchPenalty" $ \p -> if Text.null q || Text.head q == c then 0 else p

