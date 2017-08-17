module LunaStudio.Data.NodeSearcher
    ( module X
    , module LunaStudio.Data.NodeSearcher
    ) where

import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.UUID.Types         as UUID
import           LunaStudio.Data.Node    (ExpressionNode, mkExprNode)
import           Prologue                hiding (Item)
import           Text.ScopeSearcher.Item as X

mockNode :: Text -> ExpressionNode
mockNode expr = mkExprNode (unsafeFromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") expr def

entry :: Text -> (Text, Item ExpressionNode)
entry name = (name, Element $ mockNode name)

methodEntry :: Text -> [Text] -> (Text, Item ExpressionNode)
methodEntry className methodList = (className, Group (Map.fromList $ entry <$> methodList) $ mockNode className)

prepareNSData :: [Text] -> Map Text [Text] -> Items ExpressionNode
prepareNSData functions classesMap = Map.fromList $ functionsList <> methodsList where
    functionsList = entry <$> functions
    methodsList   = (uncurry methodEntry) <$> Map.toList classesMap
