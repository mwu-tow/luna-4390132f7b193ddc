module LunaStudio.Data.NodeSearcher where

import           Data.Aeson.Types        (ToJSON)
import           Data.Binary             (Binary)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Text               (Text)
import           Prologue                hiding (Item)

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
    missingImports' (NodeSearcherData imps currentImps) = filter (`Set.notMember` (Map.keysSet imps)) (convert "default" : currentImps)


