module LunaStudio.Data.NodeSearcher
    ( module X
    , module LunaStudio.Data.NodeSearcher
    ) where

import           Data.Aeson.Types        (ToJSON)
import           Data.Binary             (Binary)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.UUID.Types         as UUID
import           LunaStudio.Data.Node    (ExpressionNode, mkExprNode)
import           Prologue                hiding (Item)
import           Text.ScopeSearcher.Item as X

data ModuleHints = ModuleHints { _functions :: [Text]
					     	   , _classes   :: Map Text [Text]
							   } deriving (Eq, Generic, Show)

makeLenses ''ModuleHints
instance Binary ModuleHints
instance NFData ModuleHints
instance ToJSON ModuleHints


type ImportName   = Text
type ImportsHints = Map ImportName ModuleHints

data NodeSearcherData = NodeSearcherData { _imports        :: Map ImportName (Items ExpressionNode)
                                         , _currentImports :: [ImportName]
                                         } deriving (Eq, Generic, Show)

makeLenses ''NodeSearcherData
instance Binary  NodeSearcherData
instance NFData  NodeSearcherData
instance Default NodeSearcherData where def = NodeSearcherData def def

missingImports :: Getter NodeSearcherData [ImportName]
missingImports = to missingImports' where
    missingImports' (NodeSearcherData imps currentImps) = filter (`Set.notMember` (Map.keysSet imps)) (convert "default" : currentImps)

mockNode :: Text -> ExpressionNode
mockNode expr = mkExprNode (unsafeFromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") expr def

entry :: Text -> (Text, Item ExpressionNode)
entry name = (name, Element $ mockNode name)

methodEntry :: Text -> [Text] -> (Text, Item ExpressionNode)
methodEntry className methodList = (className, Group (Map.fromList $ entry <$> methodList) $ mockNode className)

prepareNSData :: ModuleHints -> Items ExpressionNode
prepareNSData mh = Map.fromList $ functionsList <> methodsList where
    functionsList = entry <$> (mh ^. functions)
    methodsList   = (uncurry methodEntry) <$> Map.toList (mh ^. classes)
