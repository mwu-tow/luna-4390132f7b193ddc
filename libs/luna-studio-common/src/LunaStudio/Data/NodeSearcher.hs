{-# LANGUAGE OverloadedStrings #-}
module LunaStudio.Data.NodeSearcher
    ( module LunaStudio.Data.NodeSearcher
    , Entry (..)
    , EntryType (..)
    , ClassName
    , Indices
    , Query
    , Score
    , name
    , entryType
    , weight
    , score
    , match
    , exactMatch
    , className
    ) where

import           Data.Aeson.Types (ToJSON)
import           Data.Binary      (Binary)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)
import           FuzzyText        (ClassName, Entry (..), EntryType (..), Indices, Query, Score, className, entryType, exactMatch,
                                   fuzzySearch, match, name, score, weight)
import           Prologue         hiding (Item)


data ClassHints = ClassHints { _constructors :: [Text]
                             , _methods      :: [Text]
                             } deriving (Eq, Generic, Show)

data ModuleHints = ModuleHints { _functions    :: [Text]
                               , _classes      :: Map Text ClassHints
                               } deriving (Eq, Generic, Show)

makeLenses ''ClassHints
makeLenses ''ModuleHints
instance Binary ModuleHints
instance NFData ModuleHints
instance ToJSON ModuleHints
instance Binary ClassHints
instance NFData ClassHints
instance ToJSON ClassHints


type ImportName   = Text
type ImportsHints = Map ImportName ModuleHints

data NodeSearcherData = NodeSearcherData { _imports        :: Map ImportName ModuleHints
                                         , _currentImports :: [ImportName]
                                         } deriving (Eq, Generic, Show)

makeLenses ''NodeSearcherData
instance Binary  NodeSearcherData
instance NFData  NodeSearcherData
instance Default NodeSearcherData where def = NodeSearcherData def def

missingImports :: Getter NodeSearcherData [ImportName]
missingImports = to missingImports' where
    missingImports' (NodeSearcherData imps currentImps) = filter (`Set.notMember` (Map.keysSet imps)) currentImps


data TypePreferation = TypePreferation { _localFunctionsWeight         :: Double
                                       , _globalFunctionsWeight        :: Double
                                       , _specialWeightForClassMethods :: (Set ClassName, Double)
                                       , _methodsWeight                :: Double
                                       , _constructorsWeight           :: Double
                                       } deriving Show

makeLenses ''TypePreferation
instance Default TypePreferation where def = TypePreferation 1 1 (def, 1) 1 1

searchCommands :: Query -> [Text] -> [Entry]
searchCommands q = fuzzySearch q . map (\c -> Entry c Command 1 def def True)

search :: Query -> Map ImportName ModuleHints -> Maybe TypePreferation -> [Entry]
search q h tp = fuzzySearch q $ toEntries h (fromJust def tp)


toEntries :: Map ImportName ModuleHints -> TypePreferation -> [Entry]
toEntries ih tPref = concat . Map.elems $ Map.mapWithKey moduleHintsToEntries ih where
    moduleHintsToEntries :: ImportName -> ModuleHints -> [Entry]
    moduleHintsToEntries impName mh = classesToEntries impName (mh ^. classes) <> functionsToEntries impName (mh ^. functions)
    classesToEntries :: ImportName -> Map Text ClassHints -> [Entry]
    classesToEntries impName = concat . Map.elems . Map.mapWithKey (classToEntries impName)
    classToEntries :: ImportName -> ClassName -> ClassHints -> [Entry]
    classToEntries impName cName c = methodsToEntries impName cName (c ^. methods) <> constructorsToEntries impName cName (c ^. constructors)
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
