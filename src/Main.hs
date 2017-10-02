module Main where

import qualified Data.List                    as List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Text                    as Text
import           LunaStudio.Data.NodeSearcher (ImportName, ModuleHints)
import qualified LunaStudio.Data.NodeSearcher as NS
import           Prologue

type Score   = Int
type Indices = (Int, Int)

data Entry = Entry { _name      :: Text
                   , _className :: Maybe Text
                   , _weight    :: Double 
                   , _score     :: Score
                   , _match     :: [Indices]
                   } deriving (Show, Eq)

makeLenses ''Entry

instance Ord Entry where
    e1 `compare` e2 = (fromIntegral (e1 ^. score) * (e1 ^. weight)) `compare` (fromIntegral (e2 ^. score) * (e2 ^. weight))

type Query = Text
data FuzzySearch = FuzzySearch { _entries :: [Entry]
                               , _query   :: Query
                               } deriving Show

makeLenses ''FuzzySearch


toEntries :: Map ImportName ModuleHints -> Bool -> [Entry]
toEntries ih preferMethods = concat . Map.elems $ Map.map moduleHintsToEntries ih where
    methodWeight :: Double
    methodWeight   = if preferMethods then 0.7 else 0.3
    functionWeight :: Double
    functionWeight = if preferMethods then 0.3 else 0.7
    moduleHintsToEntries :: ModuleHints -> [Entry]
    moduleHintsToEntries mh = classesToEntries (mh ^. NS.classes) <> functionsToEntries (mh ^. NS.functions)
    classesToEntries :: Map Text [Text] -> [Entry]
    classesToEntries  = concat . Map.elems . Map.mapWithKey (\k m -> map (methodToEntry k) m)
    methodToEntry :: Text -> Text -> Entry
    methodToEntry k m = Entry m (Just k) methodWeight def def
    functionsToEntries :: [Text] -> [Entry]
    functionsToEntries = map functionToEntry
    functionToEntry :: Text -> Entry
    functionToEntry f = Entry f def functionWeight def def

mockImports :: Map ImportName ModuleHints
mockImports = Map.fromList [(Text.pack "Base", mockBase), (Text.pack "Http", mockHttp)] where
    mockBase = NS.ModuleHints [Text.pack "abs", Text.pack "plus"] $ Map.fromList [(Text.pack "Int", [Text.pack "plus", Text.pack "minus", Text.pack "average", Text.pack "multiply"]), (Text.pack "Text", [Text.pack "append", Text.pack "length", Text.pack "capitalizeLetters", Text.pack "isEmpty"])]
    mockHttp = NS.ModuleHints [Text.pack "httpGet"] $ Map.fromList [(Text.pack "Http", [Text.pack "get", Text.pack "post", Text.pack "getJSON"])]

main :: IO ()
main = do
    pprint . List.sort . processEntries (Text.pack "pls") $ toEntries mockImports True

processEntries :: Query -> [Entry] -> [Entry]
processEntries q e = map updateEntry e where
    updateEntry e' = do
        let (sc, m) = getScoreAndMatch (e' ^. name)
            mergeMatch [] inds = [inds]
            mergeMatch res@((beg1, end1):t) (beg2, end2) = if end1 + 1 == beg2 then (beg1, end2) : t else (beg2, end2) : res
        e' & score .~ sc
           & match .~ reverse (foldl mergeMatch [] m)
    getScoreAndMatch n = processEntry q $ zip (Text.unpack n) [1..]

maxScore :: (Score, [Indices]) -> (Score, [Indices]) -> (Score, [Indices])
maxScore (s1, i1) (s2, i2) = if s1 >= s2 then (s1, i1) else (s2, i2)

mergeScore :: (Score, [Indices]) -> (Score, [Indices]) -> (Score, [Indices])
mergeScore (s1, i1) (s2, i2) = if null i1 then (s1+s2, i2)
    else if null i2 then (s1+s2, i1)
    else if snd (List.last i1) + 1 /= fst (List.head i2) then (s1+s2, i1 <> i2)
    else do
        let p :: [Indices] 
            p = [(fst $ List.last i1, snd $ List.head i2)]
        (s1+s2+1, List.init i1 <> p <> List.tail i2)


processEntry :: Query -> [(Char, Int)] -> (Score, [Indices])
processEntry q e = if Text.null q then (1, []) else if null e then (0, []) else do
    let bestMatch :: [(Char, Int)] -> (Score, [Indices])
        bestMatch e' = do
            let stripped :: [(Char, Int)]
                stripped = dropWhile (\(c, ind) -> c /= Text.head q) e'
            case stripped of
                []       -> (0, [])
                (_, i):t -> do
                    let nextMatch = bestMatch t
                        restMatch = mergeScore (1, [(i, i)]) $ processEntry (Text.tail q) t
                    maxScore nextMatch restMatch
    bestMatch e