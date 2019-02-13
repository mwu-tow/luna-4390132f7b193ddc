{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module NodeEditor.React.Model.Node
    ( module X
    , module NodeEditor.React.Model.Node
    ) where

import           Common.Prelude
import           Data.HashMap.Strict                        (HashMap)
import           LunaStudio.Data.Node                       as X (NodeId)
import           LunaStudio.Data.NodeLoc                    as X (NodeLoc (NodeLoc), NodePath (NodePath))
import           NodeEditor.React.Model.IsNode              as X
import           NodeEditor.React.Model.Node.ExpressionNode as X (ExpressionNode, ExpressionNodesMap)
import           NodeEditor.React.Model.Node.SidebarNode    as X (InputNode (InputNode), InputNodesMap, OutputNode (OutputNode),
                                                                  OutputNodesMap)


data Node = Expression ExpressionNode
          | Input      InputNode
          | Output     OutputNode
          deriving (Eq, Generic, NFData, Show)

makeLenses ''Node
makePrisms ''Node

type NodesMap = HashMap NodeId Node

instance HasNodeLoc Node where
    nodeLoc f (Expression node) = Expression <$> nodeLoc f node
    nodeLoc f (Input      node) = Input      <$> nodeLoc f node
    nodeLoc f (Output     node) = Output     <$> nodeLoc f node

instance HasPorts Node where
    inPortsList        (Expression node) = inPortsList  node
    inPortsList        (Input      node) = inPortsList  node
    inPortsList        (Output     node) = inPortsList  node
    outPortsList       (Expression node) = outPortsList node
    outPortsList       (Input      node) = outPortsList node
    outPortsList       (Output     node) = outPortsList node
    inPortAt  portId f (Expression node) = Expression <$> inPortAt  portId f node
    inPortAt  portId f (Input      node) = Input      <$> inPortAt  portId f node
    inPortAt  portId f (Output     node) = Output     <$> inPortAt  portId f node
    outPortAt portId f (Expression node) = Expression <$> outPortAt portId f node
    outPortAt portId f (Input      node) = Input      <$> outPortAt portId f node
    outPortAt portId f (Output     node) = Output     <$> outPortAt portId f node
    portModeAt portId f (Expression node) = Expression <$> portModeAt portId f node
    portModeAt portId f (Input      node) = Input      <$> portModeAt portId f node
    portModeAt portId f (Output     node) = Output     <$> portModeAt portId f node
