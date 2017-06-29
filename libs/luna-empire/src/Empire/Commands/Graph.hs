{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Empire.Commands.Graph
    ( addNode
    , addNodeCondTC
    , addPort
    , addPortWithConnections
    , addSubgraph
    , autolayout
    , autolayoutNodes
    , removeNodes
    , movePort
    , removePort
    , renamePort
    , setNodeExpression
    , setNodeMeta
    , setNodePosition
    , connect
    , connectPersistent
    , connectCondTC
    , connectNoTC
    , decodeLocation
    , disconnect
    , getNodeMeta
    , getBuffer
    , getCode
    , getGraph
    , getGraphNoTC
    , getNodes
    , getConnections
    , setPortDefault
    , getPortDefault
    , renameNode
    , dumpGraphViz
    , openFile
    , typecheck
    , substituteCode
    , substituteCodeFromPoints
    , loadCode
    , markerCodeSpan
    , readCodeSpan
    , getNodeIdForMarker
    , updateCodeSpan
    , withTC
    , withGraph
    , runTC
    ) where

import           Control.Arrow                    ((&&&))
import           Control.Monad                    (forM, forM_)
import           Control.Monad.Catch              (handle, onException)
import           Control.Monad.State              hiding (when)
import           Data.Coerce                      (coerce)
import           Data.Foldable                    (toList)
import           Data.List                        (elemIndex, group, sortOn)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text
import qualified Data.UUID.V4                     as UUID (nextRandom)
import           Empire.Prelude                   hiding (toList)
import qualified Safe

import           Empire.Data.AST                  (InvalidConnectionException (..), NodeRef, NotInputEdgeException (..), SomeASTException,
                                                   astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Data.Layers               (Marker, SpanLength, SpanOffset)

import           Empire.ASTOp                     (ASTOp, putNewIR, runASTOp, runAliasAnalysis)
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem, Named)
import qualified LunaStudio.Data.Breadcrumb       as Breadcrumb
import           LunaStudio.Data.Connection       (Connection (..))
import qualified LunaStudio.Data.Graph            as APIGraph
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import           LunaStudio.Data.Node             (ExpressionNode (..), InputSidebar (..), NodeId)
import qualified LunaStudio.Data.Node             as Node
import           LunaStudio.Data.NodeLoc          (NodeLoc (..))
import qualified LunaStudio.Data.NodeLoc          as NodeLoc
import           LunaStudio.Data.NodeMeta         (NodeMeta)
import qualified LunaStudio.Data.NodeMeta         as NodeMeta
import           LunaStudio.Data.Point            (Point)
import           LunaStudio.Data.Port             (InPortIndex (..), OutPortId, getPortNumber)
import           LunaStudio.Data.PortDefault      (PortDefault)
import           LunaStudio.Data.PortRef          (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.PortRef          as PortRef
import           LunaStudio.Data.Position         (Position)
import qualified LunaStudio.Data.Position         as Position

import qualified Empire.ASTOps.Builder            as ASTBuilder
import qualified Empire.ASTOps.Deconstruct        as ASTDeconstruct
import qualified Empire.ASTOps.Modify             as ASTModify
import qualified Empire.ASTOps.Parse              as ASTParse
import qualified Empire.ASTOps.Print              as ASTPrint
import qualified Empire.ASTOps.Read               as ASTRead
import qualified Empire.ASTOps.Remove             as ASTRemove
import qualified Empire.Commands.AST              as AST
import qualified Empire.Commands.Autolayout       as Autolayout
import           Empire.Commands.Breadcrumb       (withBreadcrumb)
import qualified Empire.Commands.Code             as Code
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.GraphUtils       as GraphUtils
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Empire

import           Data.Text.Position               (Delta)
import           Data.Text.Span                   (LeftSpacedSpan (..), SpacedSpan (..), leftSpacedSpan)
import qualified Luna.IR                          as IR
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Marker   (MarkedExprMap (..))
import qualified Luna.Syntax.Text.Parser.Marker   as Luna
import qualified OCI.IR.Combinators               as IR (replace, replaceSource, substitute)


addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNode = addNodeCondTC True

addNodeCondTC :: Bool -> GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNodeCondTC True  loc uuid expr meta = withTC loc False $ addNodeNoTC loc uuid expr Nothing meta
addNodeCondTC False loc@(GraphLocation f _) uuid expr meta = do
    node <- withGraph loc $ addNodeNoTC loc uuid expr Nothing meta
    withGraph (GraphLocation f $ Breadcrumb []) runAliasAnalysis
    resendCode loc
    return node

addNodeNoTC :: GraphLocation -> NodeId -> Text -> Maybe Text -> NodeMeta -> Command Graph ExpressionNode
addNodeNoTC loc uuid input name meta = do
    let propInput = Text.strip input
    parse <- fst <$> ASTParse.runParser propInput
    expr <- runASTOp $ do
        Code.propagateLengths parse
        (parsedNode, newName) <- AST.addNode uuid name parse
        index        <- getNextExprMarker
        marker       <- IR.marker' index
        IR.putLayer @SpanLength marker $ convert $ Text.length $ Code.makeMarker index
        markedNode   <- IR.marked' marker parsedNode
        [l, r]       <- IR.inputs markedNode
        IR.putLayer @SpanOffset l 0
        IR.putLayer @SpanOffset r 0
        addExprMapping index markedNode
        let textExpr = Code.makeMarker index <> maybe mempty (<> " = ") newName <> propInput
        let nodeSpan = fromIntegral $ Text.length textExpr
        IR.putLayer @SpanLength markedNode nodeSpan
        putInSequence markedNode textExpr meta
        putIntoHierarchy uuid markedNode
        return markedNode
    runAliasAnalysis
    node <- runASTOp $ do
        putChildrenIntoHierarchy uuid expr
        AST.writeMeta expr meta
        node <- GraphBuilder.buildNode uuid
        return node
    return node

distanceTo :: (Double, Double) -> (Double, Double) -> Double
distanceTo (xRef, yRef) (xPoint, yPoint) = sqrt $ (xRef - xPoint) ** 2 + (yRef - yPoint) ** 2

findPreviousNodeInSequence :: ASTOp m => NodeMeta -> [(NodeRef, NodeMeta)] -> m (Maybe NodeRef)
findPreviousNodeInSequence meta nodes = do
    let position           = Position.toTuple $ view NodeMeta.position meta
        nodesWithPositions = map (\(n, m) -> (n, Position.toTuple $ m ^. NodeMeta.position)) nodes
        nodesToTheLeft     = filter (\(n, (x, y)) -> x < fst position || (x == fst position && y < snd position)) nodesWithPositions
        nearestNode        = listToMaybe $ sortOn (\(n, p) -> distanceTo position p) nodesToTheLeft
    return $ fmap fst nearestNode

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
     b <- p x
     if b then return (Just x) else findM p xs

setSeqOffsets :: ASTOp m => NodeRef -> Delta -> Delta -> m ()
setSeqOffsets node loff roff = do
    [l, r] <- IR.inputs node
    IR.putLayer @SpanOffset l loff
    IR.putLayer @SpanOffset r roff

insertAfter :: ASTOp m => NodeRef -> Maybe NodeRef -> NodeRef -> Delta -> Text -> m (NodeRef, Bool)
insertAfter s after new textBeginning code = IR.matchExpr s $ \case
        IR.Seq l r -> do
            rt <- IR.source r
            if Just rt == after
                then do
                    indentBy <- Code.getCurrentIndentationLength
                    Code.insertAt (fromIntegral textBeginning) ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
                    newSeq <- IR.generalize <$> IR.seq s new
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return (newSeq, True)
                else do
                    lt     <- IR.source l
                    rlen   <- IR.getLayer @SpanLength rt
                    roff   <- IR.getLayer @SpanOffset r
                    (res, shouldUpdate) <- insertAfter lt after new (textBeginning - rlen - roff) code
                    when shouldUpdate (IR.replaceSource res l)
                    return (res, False)
        _ -> do
            indentBy <- Code.getCurrentIndentationLength
            if after == Just s
                then do
                    Code.insertAt (fromIntegral textBeginning) ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
                    newSeq <- IR.generalize <$> IR.seq s new
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return $ (newSeq, True)
                else do
                    slen <- IR.getLayer @SpanLength s
                    Code.insertAt (fromIntegral $ textBeginning - slen) (code <> "\n" <> Text.replicate (fromIntegral indentBy) " ")
                    newSeq <- IR.generalize <$> IR.seq new s
                    setSeqOffsets newSeq 0 (indentBy + 1)
                    return $ (newSeq, True)


putInSequence :: ASTOp m => NodeRef -> Text -> NodeMeta -> m ()
putInSequence ref code meta = do
    oldSeq      <- preuse $ Graph.breadcrumbHierarchy . BH.body
    case oldSeq of
        Just s -> do
            nodes              <- AST.readSeq s
            nodesAndMetas      <- mapM (\n -> (n,) <$> AST.readMeta n) nodes
            let nodesWithMetas =  mapMaybe (\(n,m) -> (n,) <$> m) nodesAndMetas
            nearestNode        <- findPreviousNodeInSequence meta nodesWithMetas
            blockEnd           <- Code.getCurrentBlockEnd
            (newS, shouldUpdate) <- insertAfter s nearestNode ref blockEnd code
            when shouldUpdate (updateGraphSeq $ Just newS)
            Code.gossipLengthsChanged newS
        _ -> do
            blockEnd <- Code.getCurrentBlockEnd
            Code.insertAt (fromIntegral blockEnd) $ code <> "\n"
            updateGraphSeq $ Just ref

addExprMapping :: ASTOp m => Word64 -> NodeRef -> m ()
addExprMapping index ref = do
    exprMap    <- getExprMap
    let newMap = exprMap & at index ?~ ref
    setExprMap newMap

getNextExprMarker :: ASTOp m => m Word64
getNextExprMarker = do
    exprMap <- getExprMap
    let keys         = Map.keys exprMap
        highestIndex = Safe.maximumMay keys
    return $ maybe 0 succ highestIndex

addCodeMarker :: ASTOp m => NodeRef -> m NodeRef
addCodeMarker ref = do
    index  <- getNextExprMarker
    marker <- IR.marker' index
    IR.putLayer @SpanLength marker $ convert $ Text.length $ Code.makeMarker index
    dummyBl    <- IR.blank
    markedNode <- IR.marked' marker dummyBl
    [l, r]     <- IR.inputs markedNode
    IR.putLayer @SpanOffset l 0
    IR.putLayer @SpanOffset r 0
    addExprMapping index markedNode
    Just beg <- Code.getOffsetRelativeToFile ref
    Code.insertAt beg (Code.makeMarker index)
    IR.substitute markedNode ref
    IR.replace ref dummyBl
    Code.gossipUsesChangedBy (fromIntegral $ Text.length $ Code.makeMarker index) markedNode
    return markedNode

makeTopBreadcrumbHierarchy :: ASTOp m => Map Word64 NodeId -> NodeRef -> m BH.TopItem
makeTopBreadcrumbHierarchy previousNodeIds ref = do
    let bareItem = BH.TopItem def $ Just ref
    children <- childrenFromSeq previousNodeIds ref
    return $ bareItem & BH.children .~ children

getMarker :: ASTOp m => NodeRef -> m Word64
getMarker marker = do
    IR.matchExpr marker $ \case
        IR.Marker index -> return index

childrenFromSeq :: ASTOp m => Map Word64 NodeId -> NodeRef -> m (Map NodeId BH.BChild)
childrenFromSeq previousNodeIds ref = do
    IR.matchExpr ref $ \case
        IR.Seq    l r    -> Map.union <$> (childrenFromSeq previousNodeIds =<< IR.source l) <*> (childrenFromSeq previousNodeIds =<< IR.source r)
        IR.Marked m expr -> do
            expr'      <- IR.source expr
            index      <- getMarker =<< IR.source m
            newNodeId  <- liftIO UUID.nextRandom
            let nodeId = Map.lookup index previousNodeIds
                uid    = fromMaybe newNodeId nodeId
            childTarget <- IR.matchExpr expr' $ \case
                IR.Unify l r -> do
                    ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
                    IR.source r
                _ -> do
                    IR.putLayer @Marker expr' $ Just $ OutPortRef (NodeLoc def uid) []
                    return expr'
            child <- prepareChild previousNodeIds ref childTarget
            return $ Map.singleton uid child
        _ -> childrenFromSeq previousNodeIds =<< addCodeMarker ref

lambdaChildren :: ASTOp m => Map Word64 NodeId -> NodeRef -> m (Map NodeId BH.BChild)
lambdaChildren previousNodeIds ref = IR.matchExpr ref $ \case
    IR.Seq l r -> Map.union <$> (childrenFromSeq previousNodeIds =<< IR.source l) <*> (lambdaChildren previousNodeIds =<< IR.source r)
    _          -> do
        marker <- IR.getLayer @Marker ref
        case marker of
            Just a  -> return Map.empty
            Nothing -> childrenFromSeq previousNodeIds ref

prepareChild :: ASTOp m => Map Word64 NodeId -> NodeRef -> NodeRef -> m BH.BChild
prepareChild previousNodeIds marked ref = do
    isLambda <- ASTRead.isLambda ref
    (if isLambda then fmap BH.LambdaChild .: (prepareLambdaChild previousNodeIds) else prepareExprChild previousNodeIds) marked ref

prepareChildWhenLambda :: ASTOp m => Map Word64 NodeId -> NodeRef -> NodeRef -> m (Maybe BH.LamItem)
prepareChildWhenLambda previousNodeIds marked ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild previousNodeIds marked ref else return Nothing

prepareLambdaChild :: ASTOp m => Map Word64 NodeId -> NodeRef -> NodeRef -> m BH.LamItem
prepareLambdaChild previousNodeIds marked ref = do
    portMapping  <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    lambdaBody   <- ASTRead.getFirstNonLambdaRef ref
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    children     <- lambdaChildren previousNodeIds lambdaBody
    newBody      <- ASTRead.getFirstNonLambdaRef ref
    return $ BH.LamItem portMapping marked children newBody

prepareExprChild :: ASTOp m => Map Word64 NodeId -> NodeRef -> NodeRef -> m BH.BChild
prepareExprChild previousNodeIds marked ref = do
    let bareItem = BH.ExprItem Map.empty marked
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry (prepareChildWhenLambda previousNodeIds) . (id &&& id)) args
    let addItem par (port, child) = case child of
          Just ch -> par & BH.portChildren . at port ?~ ch
          _       -> par
    return $ BH.ExprChild $ foldl addItem bareItem $ zip [0..] items

updateNodeSequenceWithOutput :: ASTOp m => Maybe NodeRef -> m ()
updateNodeSequenceWithOutput outputRef = do
    newSeq <- makeCurrentSeq outputRef
    updateGraphSeq newSeq

makeCurrentSeq :: ASTOp m => Maybe NodeRef -> m (Maybe NodeRef)
makeCurrentSeq out = do
  allNodes    <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
  sortedRefs  <- AST.sortByPosition allNodes
  let withOut = fmap head $ group $ sortedRefs ++ toList out
  AST.makeSeq withOut

updateGraphSeq :: ASTOp m => Maybe NodeRef -> m ()
updateGraphSeq newOut = do
    oldSeq     <- preuse $ Graph.breadcrumbHierarchy . BH.body
    currentTgt <- ASTRead.getCurrentASTTarget
    outLink    <- mapM ASTRead.getFirstNonLambdaLink currentTgt
    case (,) <$> outLink <*> newOut of
        Just (l, o) -> IR.replaceSource o l
        Nothing     -> return ()
    forM_ oldSeq $ flip IR.deepDeleteWithWhitelist $ Set.fromList $ maybeToList newOut
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody .= newOut
    forM_ newOut $ (Graph.breadcrumbHierarchy . BH.body .=)
    {-forM_ newOut $ updateCodeSpan-}

updateCodeSpan' :: ASTOp m => NodeRef -> m _
updateCodeSpan' ref = IR.matchExpr ref $ \case
    IR.Seq l r -> do
        l' <- updateCodeSpan' =<< IR.source l
        r' <- updateCodeSpan' =<< IR.source r
        let span = l' <> r'
        setCodeSpan ref span
        return span
    _ -> readCodeSpan ref

updateCodeSpan :: ASTOp m => NodeRef -> m ()
updateCodeSpan ref = do
    updateCodeSpan' ref
    LeftSpacedSpan (SpacedSpan off len) <- readCodeSpan ref
    setCodeSpan ref (leftSpacedSpan 14 len)

addPort :: GraphLocation -> OutPortRef -> Empire InputSidebar
addPort loc portRef = withTC loc False $ addPortNoTC loc portRef

addPortNoTC :: GraphLocation -> OutPortRef -> Command Graph InputSidebar
addPortNoTC loc (OutPortRef nl pid) = runASTOp $ do
    let nid      = nl ^. NodeLoc.nodeId
        position = getPortNumber pid
    edges <- GraphBuilder.getEdgePortMapping
    when ((fst <$> edges) /= Just nid) $ throwM NotInputEdgeException
    Just ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    ASTModify.addLambdaArg position ref
    newLam  <- ASTRead.getCurrentASTTarget
    mapM_ (ASTBuilder.attachNodeMarkersForArgs nid []) newLam
    GraphBuilder.buildInputSidebar nid

addPortWithConnections :: GraphLocation -> OutPortRef -> [AnyPortRef] -> Empire InputSidebar
addPortWithConnections loc portRef connectTo = withTC loc False $ do
    newPorts <- addPortNoTC loc portRef
    forM_ connectTo $ connectNoTC loc portRef
    return newPorts

addSubgraph :: GraphLocation -> [ExpressionNode] -> [Connection] -> Empire [ExpressionNode]
addSubgraph loc nodes conns = withTC loc False $ do
    newNodes <- forM nodes $ \n -> addNodeNoTC loc (n ^. Node.nodeId) (n ^. Node.expression) (n ^. Node.name) (n ^. Node.nodeMeta)
    forM_ conns $ \(Connection src dst) -> connectNoTC loc src (InPortRef' dst)
    return newNodes

removeNodes :: GraphLocation -> [NodeId] -> Empire ()
removeNodes loc@(GraphLocation file _) nodeIds = do
    withTC loc False $ runASTOp $ mapM removeNodeNoTC nodeIds
    resendCode loc

deepRemoveExprMarkers :: ASTOp m => BH.BChild -> m ()
deepRemoveExprMarkers chld = do
    removeExprMarker $ chld ^. BH.self
    traverseOf_ (BH._LambdaChild . BH.children     . traverse) deepRemoveExprMarkers chld
    traverseOf_ (BH._ExprChild   . BH.portChildren . traverse . re BH._LambdaChild) deepRemoveExprMarkers chld

removeNodeNoTC :: ASTOp m => NodeId -> m [NodeId]
removeNodeNoTC nodeId = do
    astRef        <- ASTRead.getASTRef nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    mapM deepRemoveExprMarkers =<< use (Graph.breadcrumbHierarchy . BH.children . at nodeId)
    Graph.breadcrumbHierarchy . BH.children . at nodeId .= Nothing
    removeFromSequence astRef
    return $ map (view PortRef.dstNodeId) obsoleteEdges

removeExprMarker :: ASTOp m => NodeRef -> m ()
removeExprMarker ref = do
    exprMap <- getExprMap
    let newExprMap = Map.filter (/= ref) exprMap
    setExprMap newExprMap

removeSequenceElement :: ASTOp m => NodeRef -> NodeRef -> m (Maybe NodeRef, Bool)
removeSequenceElement seq ref = IR.matchExpr seq $ \case
    IR.Seq l r -> do
        rt <- IR.source r
        if rt == ref
            then do
                Just offset <- Code.getOffsetRelativeToFile ref
                len         <- IR.getLayer @SpanLength ref
                edgeOff     <- IR.getLayer @SpanOffset r
                Code.removeAt (offset - edgeOff) (offset + len)
                lt          <- IR.source l
                return (Just lt, True)
            else do
                lt    <- IR.source l
                recur <- removeSequenceElement lt ref
                case recur of
                    (Just newRef, True) -> do -- left child changed
                        IR.replace newRef lt
                        return (Just newRef, False)
                    (Nothing, True)     -> do -- left child removed, right child replaces whole seq
                        roff     <- IR.getLayer @SpanOffset r
                        loff     <- IR.getLayer @SpanOffset l
                        Just pos <- Code.getOffsetRelativeToFile ref
                        Code.removeAt pos (pos + roff + loff)
                        return (Just rt, True)
                    (res, False)        -> return (res, False)
    _ -> do
        Just offset <- Code.getOffsetRelativeToFile ref
        len         <- IR.getLayer @SpanLength ref
        Code.removeAt offset (offset + len)
        return (Nothing, True)


removeFromSequence :: ASTOp m => NodeRef -> m ()
removeFromSequence ref = do
    Just oldSeq <- preuse $ Graph.breadcrumbHierarchy . BH.body
    (newS, shouldUpdate) <- removeSequenceElement oldSeq ref
    when shouldUpdate (updateGraphSeq newS)
    mapM_ Code.gossipLengthsChanged newS

removePort :: GraphLocation -> OutPortRef -> Empire InputSidebar
removePort loc portRef = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    edges <- GraphBuilder.getEdgePortMapping
    newRef <- case edges of
        Just (input, _output) -> do
            if nodeId == input then ASTModify.removeLambdaArg (portRef ^. PortRef.srcPortId) ref
                               else throwM NotInputEdgeException
        _ -> return ref
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] newRef
    GraphBuilder.buildInputSidebar nodeId

movePort :: GraphLocation -> OutPortRef -> Int -> Empire InputSidebar
movePort loc portRef newPosition = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    newRef      <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.moveLambdaArg (portRef ^. PortRef.srcPortId) newPosition ref
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] ref
    GraphBuilder.buildInputSidebar nodeId

renamePort :: GraphLocation -> OutPortRef -> Text -> Empire InputSidebar
renamePort loc portRef newName = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    Just ref    <- ASTRead.getCurrentASTTarget
    edges       <- GraphBuilder.getEdgePortMapping
    _newRef     <- case edges of
        Just (input, _) -> do
            if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.srcPortId) (Text.unpack newName) ref
                               else throwM NotInputEdgeException
        _ -> throwM NotInputEdgeException
    GraphBuilder.buildInputSidebar nodeId

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire ExpressionNode
setNodeExpression loc@(GraphLocation file _) nodeId expression = do
    node <- withTC loc False $ do
        oldExpr   <- runASTOp $ ASTRead.getASTTarget nodeId
        parsedRef <- view _1 <$> ASTParse.runReparser expression oldExpr
        oldRange  <- runASTOp $ do
            oldPointer    <- ASTRead.getASTRef nodeId
            Just oldBegin <- Code.getOffsetRelativeToFile oldPointer
            oldLen        <- IR.getLayer @SpanLength oldPointer
            ASTModify.rewireNode nodeId parsedRef
            return (oldBegin, oldBegin + oldLen)
        runAliasAnalysis
        (node, code)  <- runASTOp $ do
            expr      <- ASTRead.getASTPointer nodeId
            marked    <- ASTRead.getASTRef nodeId
            item      <- prepareChild Map.empty marked parsedRef
            Graph.breadcrumbHierarchy . BH.children . ix nodeId .= item
            node <- GraphBuilder.buildNode nodeId
            code <- printMarkedExpression marked
            IR.putLayer @SpanLength marked (fromIntegral $ Text.length code)
            Code.applyDiff (fst oldRange) (snd oldRange) code
            Code.gossipUsesChanged expr
            return (node, code)
        return node
    resendCode loc
    return node

updateExprMap :: ASTOp m => NodeRef -> NodeRef -> m ()
updateExprMap new old = do
    exprMap <- getExprMap
    let updated = Map.map (\a -> if a == old then new else a) exprMap
    setExprMap updated

resendCode :: GraphLocation -> Empire ()
resendCode loc@(GraphLocation file _) = do
    code <- fmap Code.removeMarkers $ withGraph loc $ use Graph.code
    Publisher.notifyCodeUpdate file
                               (Code.deltaToPoint 0 code)
                               (Code.deltaToPoint (fromIntegral $ Text.length code) code)
                               code
                               Nothing

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph loc $ runASTOp $ do
    ref <- ASTRead.getASTRef nodeId
    AST.writeMeta ref newMeta

setNodePosition :: GraphLocation -> NodeId -> Position -> Empire ()
setNodePosition loc nodeId newPos = do
    oldMeta <- fromMaybe def <$> getNodeMeta loc nodeId
    setNodeMeta loc nodeId $ oldMeta & NodeMeta.position .~ newPos

setNodePositionAST :: ASTOp m => NodeId -> Position -> m ()
setNodePositionAST nodeId newPos = do
    ref <- ASTRead.getASTRef nodeId
    oldMeta <- fromMaybe def <$> AST.readMeta ref
    AST.writeMeta ref $ oldMeta & NodeMeta.position .~ newPos

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connectCondTC True  loc outPort anyPort = connect loc outPort anyPort
connectCondTC False loc outPort anyPort = do
    connection <- withGraph loc $ connectNoTC loc outPort anyPort
    updateNodeCode loc $ anyPort ^. PortRef.nodeId
    resendCode loc
    return connection

connect :: GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connect loc outPort anyPort = do
    connection <- withTC loc False $ connectNoTC loc outPort anyPort
    resendCode loc
    return connection

connectPersistent :: ASTOp m => OutPortRef -> AnyPortRef -> m Connection
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (InPortRef' dst@(InPortRef (NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []        -> makeWhole srcNodeId dstNodeId srcPort
        [Self]    -> makeAcc   srcNodeId dstNodeId srcPort
        [Arg num] -> makeApp   srcNodeId dstNodeId num srcPort
    return $ Connection src dst
connectPersistent src@(OutPortRef (NodeLoc _ srcNodeId) srcPort) (OutPortRef' dst@(OutPortRef d@(NodeLoc _ dstNodeId) dstPort)) = do
    case dstPort of
        []    -> do
            ASTBuilder.flipNode dstNodeId
            connectPersistent src (InPortRef' (InPortRef d []))
        _ : _ -> throwM InvalidConnectionException

connectNoTC :: GraphLocation -> OutPortRef -> AnyPortRef -> Command Graph Connection
connectNoTC loc outPort anyPort = runASTOp $ connectPersistent outPort anyPort

data SelfPortDefaultException = SelfPortDefaultException InPortRef
    deriving (Show)

instance Exception SelfPortDefaultException where
    fromException = astExceptionFromException
    toException = astExceptionToException

getPortDefault :: GraphLocation -> InPortRef -> Empire (Maybe PortDefault)
getPortDefault loc port@(InPortRef  _ (Self : _))              = throwM $ SelfPortDefaultException port
getPortDefault loc (InPortRef  (NodeLoc _ nodeId) (Arg x : _)) = withGraph loc $ runASTOp $ flip GraphBuilder.getInPortDefault x =<< GraphUtils.getASTTarget nodeId

setPortDefault :: GraphLocation -> InPortRef -> Maybe PortDefault -> Empire ()
setPortDefault loc (InPortRef (NodeLoc _ nodeId) port) (Just val) = withTC loc False $ runASTOp $ do
    parsed <- ASTParse.parsePortDefault val
    refBeg <- Code.getASTTargetBeginning nodeId
    e <- ASTRead.getTargetEdge nodeId
    case port of
        [Self]    -> do
            ASTBuilder.makeAccessor parsed e refBeg
        [Arg num] -> do
            ASTBuilder.applyFunction e refBeg parsed num
setPortDefault loc port Nothing = withTC loc False $ runASTOp $ disconnectPort port

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc@(GraphLocation file _) port@(InPortRef (NodeLoc _ nid) _) = do
    nodeId <- withTC loc False $ runASTOp $ do
        disconnectPort port

        edges <- GraphBuilder.getEdgePortMapping
        return $ case edges of
            Just (input, output) -> do
                if | nid == input  -> Nothing
                   | nid == output -> Just nid
                   | otherwise     -> Just nid
            _ -> Just nid
    resendCode loc

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc nodeId = withGraph loc $ runASTOp $ do
    ref <- GraphUtils.getASTPointer nodeId
    AST.readMeta ref

getCode :: GraphLocation -> Empire String
getCode loc@(GraphLocation file _) = Text.unpack . Code.removeMarkers <$> withGraph loc (use Graph.code)

-- TODO[MK]: handle span
getBuffer :: FilePath -> Maybe (Int, Int) -> Empire Text
getBuffer file span = Code.removeMarkers <$> withGraph (GraphLocation file $ Breadcrumb []) (use Graph.code)

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC loc True $ runASTOp $ do
    exc <- use $ Graph.parseError
    case exc of
        Just e  -> throwM e
        Nothing -> GraphBuilder.buildGraph

getGraphNoTC :: GraphLocation -> Empire APIGraph.Graph
getGraphNoTC loc = withGraph loc $ runASTOp $ GraphBuilder.buildGraph

getNodes :: GraphLocation -> Empire [ExpressionNode]
getNodes loc = withTC loc True $ runASTOp $ view APIGraph.nodes <$> GraphBuilder.buildGraph

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation file crumbs) =
    withGraph (GraphLocation file $ Breadcrumb []) $ GraphBuilder.decodeBreadcrumbs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = do
    withTC loc False $ runASTOp $ do
        v <- ASTRead.getASTVar nid
        ASTModify.renameVar v $ convert name
        Code.replaceAllUses v name
    resendCode loc

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ return ()

autolayoutNodes :: ASTOp m => [NodeId] -> m ()
autolayoutNodes nids = do
    nodes <- GraphBuilder.buildNodes
    conns <- GraphBuilder.buildConnections
    let autolayout = Autolayout.autolayoutNodes nids nodes conns
    mapM_ (uncurry setNodePositionAST) autolayout

openFile :: FilePath -> Empire ()
openFile path = do
    code <- liftIO $ Text.readFile path
    Library.createLibrary Nothing path code
    let loc = GraphLocation path $ Breadcrumb []
    result <- handle (\(e :: SomeASTException) -> return $ Left e) $ fmap Right $ do
        withGraph loc $ do
            Graph.parseError .= Nothing
            loadCode code
        autolayout loc
    case result of
        Left e -> withGraph loc $ Graph.parseError ?= e
        _      -> return ()

typecheck :: GraphLocation -> Empire ()
typecheck loc = withTC loc False $ return ()

substituteCodeFromPoints :: FilePath -> Point -> Point -> Text -> Maybe Point -> Empire ()
substituteCodeFromPoints path start end code cursor = do
    let loc = GraphLocation path (Breadcrumb [])
    (s, e) <- withGraph loc $ do
        oldCode   <- use Graph.code
        let noMarkers  = Code.removeMarkers oldCode
            deltas     = (Code.pointToDelta start noMarkers, Code.pointToDelta end noMarkers)
            realDeltas = Code.viewDeltasToReal oldCode deltas
        return realDeltas
    substituteCode path s e code Nothing

substituteCode :: FilePath -> Delta -> Delta -> Text -> Maybe Delta -> Empire ()
substituteCode path start end code cursor = do
    let loc = GraphLocation path (Breadcrumb [])
    newCode <- withGraph loc $ Code.applyDiff start end code
    result <- handle (\(e :: SomeASTException) -> return $ Left e) $ fmap Right $ withGraph loc $ do
        Graph.parseError .= Nothing
        reloadCode loc newCode
    case result of
        Left e -> withGraph loc $ Graph.parseError ?= e
        _      -> return ()

reloadCode :: GraphLocation -> Text -> Command Graph ()
reloadCode loc code = do
    oldMetas <- runASTOp $ do
        m <- getExprMap
        oldMetas <- forM (Map.assocs m) $ \(marker, expr) -> (marker,) <$> AST.readMeta expr
        return [ (marker, meta) | (marker, Just meta) <- oldMetas ]
    previousNodeIds <- runASTOp $ do
        m <- getExprMap
        let markers = Map.keys m
        nodeIds <- mapM (\k -> (k,) <$> getNodeIdForMarker (fromIntegral k)) markers
        return $ Map.fromList [ (marker, nodeId) | (marker, Just nodeId) <- nodeIds ]
    oldHierarchy <- use Graph.breadcrumbHierarchy
    Graph.breadcrumbHierarchy .= def
    loadCodeWithNodeIdCache previousNodeIds code `onException` (Graph.breadcrumbHierarchy .= oldHierarchy)
    runASTOp $ do
        currentExprMap <- getExprMap
        forM_ oldMetas $ \(marker, oldMeta) -> do
            let expr = Map.lookup marker currentExprMap
            forM_ expr $ \e -> AST.writeMeta e oldMeta

putIntoHierarchy :: ASTOp m => NodeId -> NodeRef -> m ()
putIntoHierarchy nodeId marked = do
    let nodeItem = BH.ExprItem Map.empty marked
    Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem

putChildrenIntoHierarchy :: ASTOp m => NodeId -> NodeRef -> m ()
putChildrenIntoHierarchy uuid expr = do
    target       <- ASTRead.getASTTarget uuid
    marked       <- ASTRead.getASTRef uuid
    item         <- prepareChild Map.empty marked target
    Graph.breadcrumbHierarchy . BH.children . ix uuid .= item

copyMeta :: ASTOp m => NodeRef -> NodeRef -> m ()
copyMeta donor recipient = do
    meta <- AST.readMeta donor
    forM_ meta $ AST.writeMeta recipient

markNode :: ASTOp m => NodeId -> m ()
markNode nodeId = do
    var <- ASTRead.getASTMarkerPosition nodeId
    ASTBuilder.attachNodeMarkers nodeId [] var

loadCode :: Text -> Command Graph ()
loadCode = loadCodeWithNodeIdCache Map.empty

loadCodeWithNodeIdCache :: Map Word64 NodeId -> Text -> Command Graph ()
loadCodeWithNodeIdCache _ code | Text.null code = return ()
loadCodeWithNodeIdCache previousNodeIds code = do
    (ir, IR.Rooted main ref, exprMap) <- liftIO $ ASTParse.runProperParser code
    setExprMap (coerce exprMap)
    Graph.unit .= ir
    putNewIR main
    Graph.breadcrumbHierarchy . BH._ToplevelParent . BH.topBody ?= ref
    runASTOp $ Code.propagateLengths ref
    runAliasAnalysis
    newBH <- runASTOp $ makeTopBreadcrumbHierarchy previousNodeIds ref
    Graph.breadcrumbHierarchy .= BH.ToplevelParent newBH

infixl 5 |>
(|>) :: GraphLocation -> BreadcrumbItem -> GraphLocation
(|>) (GraphLocation file bc) item = GraphLocation file $ coerce $ (++ [item]) $ coerce bc

autolayout :: GraphLocation -> Empire ()
autolayout loc = do
    kids <- withGraph loc $ do
        kids <- uses Graph.breadcrumbHierarchy (view BH.children)
        runASTOp $ autolayoutNodes $ Map.keys kids
        return kids
    let next = concatMap (\(k, v) -> case v of
            BH.LambdaChild{}                -> [Breadcrumb.Lambda k]
            BH.ExprChild (BH.ExprItem pc _) -> map (Breadcrumb.Arg k) (Map.keys pc)) $ Map.assocs kids
    mapM_ (\a -> autolayout (loc |> a)) next

nodeLineById :: ASTOp m => NodeId -> m (Maybe Int)
nodeLineById nodeId = do
    nodeIds <- GraphBuilder.getNodeIdSequence
    let line = elemIndex nodeId nodeIds
    return line

nodeLine :: ASTOp m => NodeRef -> m (Maybe Int)
nodeLine ref = do
    Just nodeSeq <- GraphBuilder.getNodeSeq
    nodes        <- AST.readSeq nodeSeq
    let line = elemIndex ref nodes
    return line

getExprMap :: ASTOp m => m (Map.Map Luna.MarkerId NodeRef)
getExprMap = use Graph.codeMarkers

setExprMap :: MonadState Graph m => Map.Map Luna.MarkerId NodeRef -> m ()
setExprMap exprMap = Graph.codeMarkers .= exprMap

printMarkedExpression :: ASTOp m => NodeRef -> m Text
printMarkedExpression ref = do
    exprMap <- getExprMap
    realRef <- IR.matchExpr ref $ \case
        IR.Marked _m expr -> IR.source expr
        _                 -> return ref
    expr    <- Text.pack <$> ASTPrint.printExpression realRef
    let markers = Map.keys $ Map.filter (== ref) exprMap
        marker  = case markers of
            (index:_) -> Text.pack $ "«" ++ show index ++ "»"
            _         -> ""
    return $ Text.concat [marker, expr]

data Sidebar = SidebarInput | SidebarOutput | NotSidebar
    deriving Eq

isSidebar :: ASTOp m => NodeId -> m Sidebar
isSidebar nodeId = do
    sidebars <- GraphBuilder.getEdgePortMapping
    return $ case sidebars of
        Just (input, output) | input  == nodeId -> SidebarInput
                             | output == nodeId -> SidebarOutput
                             | otherwise        -> NotSidebar
        Nothing                                 -> NotSidebar

isInput :: ASTOp m => NodeId -> m Bool
isInput nodeId = (== SidebarInput) <$> isSidebar nodeId

isOutput :: ASTOp m => NodeId -> m Bool
isOutput nodeId = (== SidebarOutput) <$> isSidebar nodeId

updateNodeCode :: GraphLocation -> NodeId -> Empire ()
updateNodeCode loc@(GraphLocation file _) nodeId = do
    input <- withGraph loc $ runASTOp $ isInput nodeId
    if input then return () else do
        (ref, pointer) <- withGraph loc $ runASTOp $ do
            output     <- isOutput nodeId
            ref        <- if output then fromJust <$> ASTRead.getCurrentASTRef else ASTRead.getASTRef nodeId
            pointer    <- if output then fromJust <$> ASTRead.getCurrentASTPointer else ASTRead.getASTPointer nodeId
            return (ref, pointer)
        (range, expression) <- withGraph (GraphLocation file (Breadcrumb [])) $ runASTOp $ do
            range                <- readRange ref
            expression           <- printMarkedExpression ref
            LeftSpacedSpan (SpacedSpan off _) <- readCodeSpan ref
            setCodeSpan ref (leftSpacedSpan off (fromIntegral $ Text.length expression))
            return (range, expression)
        withGraph (GraphLocation file (Breadcrumb [])) $ do
            runASTOp $ do
                oldSeq      <- preuse $ Graph.breadcrumbHierarchy . BH.body
                forM_ oldSeq updateCodeSpan
            void $ Code.applyDiff (fst range) (snd range + 1) $ Text.concat [expression, "\n"]

previousOffset :: ASTOp m => NodeRef -> m Delta
previousOffset ref = do
    parents <- IR.getLayer @IR.Succs ref
    case toList parents of
        [] -> return mempty
        [parent] -> do
            inputs <- mapM IR.source =<< IR.inputs =<< IR.readTarget parent
            let lefts = takeWhile (/= ref) inputs
            spans  <- mapM readCodeSpan lefts
            let LeftSpacedSpan (SpacedSpan leftOff leftLen) = mconcat spans
            offset <- previousOffset =<< IR.readTarget parent
            LeftSpacedSpan (SpacedSpan o _) <- readCodeSpan =<< IR.readTarget parent
            return (leftOff + offset + o + leftLen)

readRangeProper :: ASTOp m => NodeRef -> m (LeftSpacedSpan Delta)
readRangeProper ref = do
    refSpan@(LeftSpacedSpan (SpacedSpan off len)) <- readCodeSpan ref
    moreOffset <- previousOffset ref
    let properOffset = off + moreOffset
        properSpan   = leftSpacedSpan properOffset len
    return properSpan

readRange :: ASTOp m => NodeRef -> m (Int, Int)
readRange ref = do
    LeftSpacedSpan (SpacedSpan offset len) <- readRangeProper ref
    return (fromIntegral offset, fromIntegral $ offset + len)

readCodeSpan :: ASTOp m => NodeRef -> m (LeftSpacedSpan Delta)
readCodeSpan ref = view CodeSpan.realSpan <$> IR.getLayer @CodeSpan ref

setCodeSpan :: ASTOp m => NodeRef -> LeftSpacedSpan Delta -> m ()
setCodeSpan ref s = IR.putLayer @CodeSpan ref $ CodeSpan.mkRealSpan s

getNodeIdForMarker :: ASTOp m => Int -> m (Maybe NodeId)
getNodeIdForMarker index = do
    exprMap      <- getExprMap
    let exprMap' :: Map.Map Luna.MarkerId NodeRef
        exprMap' = coerce exprMap
        Just ref = Map.lookup (fromIntegral index) exprMap'
    IR.matchExpr ref $ \case
        IR.Marked _m expr -> do
            expr'     <- IR.source expr
            varNodeId <- ASTRead.safeGetVarNodeId expr'
            nodeId    <- ASTRead.getNodeId expr'
            return $ varNodeId <|> nodeId

markerCodeSpan :: GraphLocation -> Int -> Empire (Int, Int)
markerCodeSpan loc index = withGraph loc $ runASTOp $ do
    exprMap      <- getExprMap
    let exprMap' :: Map.Map Luna.MarkerId NodeRef
        exprMap' = coerce exprMap
        Just ref = Map.lookup (fromIntegral index) exprMap'
    readRange ref

-- internal

runTC :: GraphLocation -> Bool -> Command Graph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

printNodeLine :: ASTOp m => NodeId -> m String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= ASTPrint.printExpression

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc@(GraphLocation file _) flush cmd = do
    res <- withGraph loc $ cmd
    withGraph (GraphLocation file $ Breadcrumb []) $ runTC loc flush
    return res

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation file breadcrumb) = withBreadcrumb file breadcrumb

getOutEdges :: ASTOp m => NodeId -> m [InPortRef]
getOutEdges nodeId = do
    edges <- GraphBuilder.buildConnections
    let filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: ASTOp m => InPortRef -> m ()
disconnectPort (InPortRef (NodeLoc _ dstNodeId) dstPort) = case dstPort of
    []        -> setToNothing dstNodeId
    [Self]    -> unAcc dstNodeId
    [Arg num] -> unApp dstNodeId num

setToNothing :: ASTOp m => NodeId -> m ()
setToNothing dst = do
    edges <- GraphBuilder.getEdgePortMapping
    let disconnectOutputEdge = case edges of
            Nothing       -> False
            Just (_, out) -> out == dst
    nothing <- IR.generalize <$> IR.cons_ "None"
    if disconnectOutputEdge
        then do
            updateNodeSequenceWithOutput (Just nothing)
            let item = BH.ExprItem Map.empty nothing
            uid <- liftIO $ UUID.nextRandom
            Graph.breadcrumbHierarchy . BH.children . at uid ?= BH.ExprChild item
            IR.putLayer @Marker nothing $ Just $ OutPortRef (NodeLoc def uid) []
        else GraphUtils.rewireNode dst nothing

unAcc :: ASTOp m => NodeId -> m ()
unAcc nodeId = do
    dstAst     <- ASTRead.getTargetEdge nodeId
    beg        <- Code.getASTTargetBeginning nodeId
    ASTBuilder.removeAccessor dstAst beg

unApp :: ASTOp m => NodeId -> Int -> m ()
unApp nodeId pos = do
    astNode <- GraphUtils.getASTTarget nodeId
    newNodeRef <- ASTRemove.removeArg astNode pos
    GraphUtils.rewireNode nodeId newNodeRef

makeAcc :: ASTOp m => NodeId -> NodeId -> OutPortId -> m ()
makeAcc src dst outPort = do
    dstBeg     <- Code.getASTTargetBeginning dst
    srcAst     <- ASTRead.getASTOutForPort src outPort
    dstAst     <- ASTRead.getTargetEdge dst
    ASTBuilder.makeAccessor srcAst dstAst dstBeg

makeApp :: ASTOp m => NodeId -> NodeId -> Int -> OutPortId -> m ()
makeApp src dst pos outPort = do
    dstBeg     <- Code.getASTTargetBeginning dst
    srcAst     <- ASTRead.getASTOutForPort src outPort
    dstAst     <- ASTRead.getTargetEdge dst
    ASTBuilder.applyFunction dstAst dstBeg srcAst pos

makeWhole :: ASTOp m => NodeId -> NodeId -> OutPortId -> m ()
makeWhole src dst outPort = do
    edges <- GraphBuilder.getEdgePortMapping
    let connectToOutputEdge = case edges of
            Nothing       -> False
            Just (_, out) -> out == dst
    srcAst <- ASTRead.getASTOutForPort src outPort
    if connectToOutputEdge then updateNodeSequenceWithOutput (Just srcAst) else GraphUtils.rewireNode dst srcAst
