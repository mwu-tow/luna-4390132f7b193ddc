{-# LANGUAGE OverloadedStrings #-}
module FuzzySearch
    ( search
    , searchCommands
    , Entry (..)
    , EntryType (..)
    , Score
    , Indices
    , ClassName
    , TypePreferation (..)
    , name
    , entryType
    , weight
    , score
    , match
    , partialMatch
    , className
    , localFunctionsWeight
    , globalFunctionsWeight
    , methodsWeight
    , constructorsWeight
    ) where

import           Data.Char
import qualified Data.List                    as List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text
import           LunaStudio.Data.NodeSearcher (ClassHints, ImportName, ModuleHints)
import qualified LunaStudio.Data.NodeSearcher as NS
import           Prologue


type Score   = Int
type Indices = (Int, Int)
type Bonus   = Int
type ClassName = Text

data TypePreferation = TypePreferation { _localFunctionsWeight         :: Double
                                       , _globalFunctionsWeight        :: Double
                                       , _specialWeightForClassMethods :: (Set ClassName, Double)
                                       , _methodsWeight                :: Double
                                       , _constructorsWeight           :: Double
                                       } deriving Show

makeLenses ''TypePreferation
instance Default TypePreferation where def = TypePreferation 1 1 (def, 1) 1 1

data EntryType = Function | Method ClassName | Constructor ClassName | Command deriving (Show, Eq)

data Entry = Entry { _name         :: Text
                   , _entryType    :: EntryType
                   , _weight       :: Double 
                   , _score        :: Score
                   , _match        :: [Indices]
                   , _partialMatch :: Bool
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
        if e1 ^. partialMatch && not (e2 ^. partialMatch) then GT
        else if not (e1 ^. partialMatch) && e2 ^. partialMatch then LT
        else if compareScore /= EQ then compareScore
        else Text.length (e1 ^. name) `compare` Text.length (e2 ^. name)

type Query = Text

bonusMap :: Map Text Bonus
bonusMap = Map.fromList [wordPrefixBonus, prefixSequenceBonus, sequenceBonus, suffixBonus, omittedLettersPenalty, mismatchPenalty]

wordPrefixBonus, prefixSequenceBonus, sequenceBonus, suffixBonus, omittedLettersPenalty, mismatchPenalty :: (Text, Bonus)
wordPrefixBonus       = ("wordPrefixBonus",       6)
sequenceBonus         = ("sequenceBonus",         10)
prefixSequenceBonus   = ("prefixSequenceBonus",   12)
suffixBonus           = ("suffixBonus",           4)
omittedLettersPenalty = ("omittedLettersPenalty", -1)
mismatchPenalty       = ("mismatchPenalty",       -5)

searchCommands :: Query -> [Text] -> [Entry]
searchCommands q = processEntries q . map (\c -> Entry c Command 1 def def True)

search :: Query -> Map ImportName ModuleHints -> Maybe TypePreferation -> [Entry]
search q h tp = processEntries q $ toEntries h (fromJust def tp)

toEntries :: Map ImportName ModuleHints -> TypePreferation -> [Entry]
toEntries ih tPref = concat . Map.elems $ Map.mapWithKey moduleHintsToEntries ih where
    moduleHintsToEntries :: ImportName -> ModuleHints -> [Entry]
    moduleHintsToEntries impName mh = classesToEntries impName (mh ^. NS.classes) <> functionsToEntries impName (mh ^. NS.functions)
    classesToEntries :: ImportName -> Map Text ClassHints -> [Entry]
    classesToEntries impName = concat . Map.elems . Map.mapWithKey (classToEntries impName)
    classToEntries :: ImportName -> ClassName -> ClassHints -> [Entry]
    classToEntries impName cName c = methodsToEntries impName cName (c ^. NS.methods) <> constructorsToEntries impName cName (c ^. NS.constructors)
    methodsToEntries :: ImportName -> ClassName -> [Text] -> [Entry]
    methodsToEntries impName cName = map (methodToEntry impName cName)
    methodToEntry :: ImportName -> ClassName -> Text -> Entry
    methodToEntry impName cName m = let et = Method cName in Entry m et (getWeight impName cName et) def def True
    constructorsToEntries :: ImportName -> ClassName -> [Text] -> [Entry]
    constructorsToEntries impName cName = map (constructorToEntry impName cName) 
    constructorToEntry :: ImportName -> ClassName -> Text -> Entry
    constructorToEntry impName cName c = let et = Constructor cName in Entry c et (getWeight impName cName et) def def True
    functionsToEntries :: ImportName -> [Text] -> [Entry]
    functionsToEntries impName = map (functionToEntry impName)
    functionToEntry :: ImportName -> Text -> Entry
    functionToEntry impName f = Entry f Function (getWeight impName def Function) def def True
    getWeight :: ImportName -> ClassName -> EntryType -> Double
    getWeight moduleName cName et = case et of
        Function      -> if moduleName == "Local" then tPref ^. localFunctionsWeight else tPref ^. globalFunctionsWeight
        Method      _ -> if Set.member cName . fst $ tPref ^. specialWeightForClassMethods
            then snd $ tPref ^. specialWeightForClassMethods
            else tPref ^. methodsWeight
        Constructor _ -> tPref ^. constructorsWeight
        _             -> def

processEntries :: Query -> [Entry] -> [Entry]
processEntries q e = List.sort $ map updateEntry e where
    updateEntry e' = do
        let (sc, m) = getScoreAndMatch (e' ^. name)
            matchLength = foldl (\s (beg, end) -> s + end - beg + 1) 0 m
        e' & score        .~ sc
           & match        .~ m
           & partialMatch .~ if matchLength == Text.length q then False else True
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

