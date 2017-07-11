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
    , withGraph'
    , withUnit
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
import qualified System.IO                        as IO

import           Empire.Data.AST                  (InvalidConnectionException (..), NodeRef, NotInputEdgeException (..), SomeASTException,
                                                   astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (ClsGraph, Graph, NodeCache(..), portMappingMap, nodeIdMap)
import qualified Empire.Data.Graph                as Graph
import           Empire.Data.Layers               (Marker, SpanLength, SpanOffset)
import qualified Empire.Data.Library              as Library

import           Empire.ASTOp                     (ClassOp, GraphOp, putNewIR, putNewIRCls, runASTOp, runAliasAnalysis)
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
import           Empire.ASTOps.BreadcrumbHierarchy (getMarker, prepareChild, makeTopBreadcrumbHierarchy)
import qualified Empire.ASTOps.Deconstruct        as ASTDeconstruct
import qualified Empire.ASTOps.Modify             as ASTModify
import qualified Empire.ASTOps.Parse              as ASTParse
import qualified Empire.ASTOps.Print              as ASTPrint
import qualified Empire.ASTOps.Read               as ASTRead
import qualified Empire.ASTOps.Remove             as ASTRemove
import qualified Empire.Commands.AST              as AST
import qualified Empire.Commands.Autolayout       as Autolayout
import           Empire.Commands.Breadcrumb       (makeGraph, makeGraphCls, withBreadcrumb)
import           Empire.Commands.Code             (addExprMapping, getNextExprMarker, getExprMap, setExprMap)
import qualified Empire.Commands.Code             as Code
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.GraphUtils       as GraphUtils
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Empire

import           Data.Text.Position               (Delta)
import           Data.Text.Span                   (LeftSpacedSpan (..), SpacedSpan (..), leftSpacedSpan)
import qualified Luna.IR                          as IR
import qualified Luna.IR.Term.Core                as Term
import qualified OCI.IR.Combinators               as IR (replaceSource, deleteSubtree, narrow, narrowTerm, replace)
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Marker   (MarkedExprMap (..))
import qualified Luna.Syntax.Text.Parser.Marker   as Luna
import qualified OCI.IR.Combinators               as IR (replace, replaceSource, substitute)


addNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNode = addNodeCondTC True

addNodeCondTC :: Bool -> GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addNodeCondTC tc loc@(GraphLocation f _) uuid expr meta
    | GraphLocation _ (Breadcrumb []) <- loc = addFunNode loc uuid expr meta
    | otherwise = do
        let runner = if tc then withTC loc False else withGraph loc
        node <- runner $ addNodeNoTC loc uuid expr Nothing meta
        resendCode loc
        return node

addFunNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Empire ExpressionNode
addFunNode loc uuid expr meta = withUnit loc $ do
    (parse, code) <- ASTParse.runFunHackParser expr
    runASTOp $ AST.writeMeta parse meta
    name <- runASTOp $ IR.matchExpr parse $ \case
        IR.ASGRootedFunction name _ -> return $ nameToString name
    klass <- use Graph.clsClass
    runASTOp $ do
        funs <- AST.classFunctions klass
        firstFunctionPosition <- Code.functionBlockStartRef (head funs)
        Code.insertAt firstFunctionPosition code
        return ()
    runASTOp $ IR.matchExpr klass $ \case
        IR.Unit _ _ cls -> do
            cls' <- IR.source cls
            Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
            l <- IR.unsafeGeneralize <$> IR.link parse cls'
            IR.modifyExprTerm cls'' $ wrapped . IR.termClsASG_decls %~ (l:)

    funs <- use Graph.clsFuns
    (uuid, graph) <- makeGraphCls parse Nothing

    runASTOp $ GraphBuilder.buildClassNode uuid name

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

findPreviousSeq :: GraphOp m => NodeRef -> Set.Set NodeRef -> m (Maybe NodeRef)
findPreviousSeq seq nodesToTheLeft = IR.matchExpr seq $ \case
    IR.Seq l r -> do
        l' <- IR.source l
        r' <- IR.source r
        if Set.member r' nodesToTheLeft then return (Just r') else findPreviousSeq l' nodesToTheLeft
    _ -> return $ if Set.member seq nodesToTheLeft then Just seq else Nothing

findPreviousNodeInSequence :: GraphOp m => NodeRef -> NodeMeta -> [(NodeRef, NodeMeta)] -> m (Maybe NodeRef)
findPreviousNodeInSequence seq meta nodes = do
    let position           = Position.toTuple $ view NodeMeta.position meta
        nodesWithPositions = map (\(n, m) -> (n, Position.toTuple $ m ^. NodeMeta.position)) nodes
        nodesToTheLeft     = filter (\(n, (x, y)) -> x <= fst position) nodesWithPositions
    findPreviousSeq seq (Set.fromList $ map fst nodesToTheLeft)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
     b <- p x
     if b then return (Just x) else findM p xs

setSeqOffsets :: GraphOp m => NodeRef -> Delta -> Delta -> m ()
setSeqOffsets node loff roff = do
    [l, r] <- IR.inputs node
    IR.putLayer @SpanOffset l loff
    IR.putLayer @SpanOffset r roff

insertAfter :: GraphOp m => NodeRef -> Maybe NodeRef -> NodeRef -> Delta -> Text -> m (NodeRef, Bool)
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


putInSequence :: GraphOp m => NodeRef -> Text -> NodeMeta -> m ()
putInSequence ref code meta = do
    oldSeq             <- use $ Graph.breadcrumbHierarchy . BH.body
    nodes              <- AST.readSeq oldSeq
    nodesAndMetas      <- mapM (\n -> (n,) <$> AST.readMeta n) nodes
    let nodesWithMetas =  mapMaybe (\(n,m) -> (n,) <$> m) nodesAndMetas
    nearestNode        <- findPreviousNodeInSequence oldSeq meta nodesWithMetas
    blockEnd           <- Code.getCurrentBlockEnd
    (newS, shouldUpdate) <- insertAfter oldSeq nearestNode ref blockEnd code
    when shouldUpdate (updateGraphSeq $ Just newS)
    Code.gossipLengthsChanged newS

addOutputAtEnd :: GraphOp m => NodeRef -> NodeRef -> Delta -> m NodeRef
addOutputAtEnd initial out blockEnd = do
    indentBy <- Code.getCurrentIndentationLength
    code     <- ASTPrint.printFullExpression out
    seq      <- IR.generalize <$> IR.seq initial out
    Code.insertAt blockEnd ("\n" <> Text.replicate (fromIntegral indentBy) " " <> code)
    setSeqOffsets seq 0 (indentBy + 1)
    return seq

reconnectOut :: GraphOp m => NodeRef -> NodeRef -> Delta -> m (Maybe NodeRef)
reconnectOut seq out blockEnd = IR.matchExpr seq $ \case
    IR.Seq l r -> do
        right <- IR.source r
        IR.matchExpr right $ \case
            IR.Marked _ _ -> Just <$> addOutputAtEnd seq out blockEnd
            _             -> do
                len <- IR.getLayer @SpanLength right
                ASTBuilder.replaceEdgeSource r (blockEnd - len) out
                return Nothing
    IR.Marked _ _ -> Just <$> addOutputAtEnd seq out blockEnd
    _             -> do
        len  <- IR.getLayer @SpanLength seq
        code <- ASTPrint.printFullExpression out
        Code.applyDiff (blockEnd - len) blockEnd code
        return $ Just out

setOutputTo :: GraphOp m => NodeRef -> m ()
setOutputTo out = do
    oldSeq   <- use $ Graph.breadcrumbHierarchy . BH.body
    blockEnd <- Code.getCurrentBlockEnd
    newSeq   <- reconnectOut oldSeq out blockEnd
    mapM_ (updateGraphSeq . Just)   newSeq
    mapM_ Code.gossipUsesChanged newSeq

updateGraphSeq :: GraphOp m => Maybe NodeRef -> m ()
updateGraphSeq newOut = do
    oldSeq     <- use $ Graph.breadcrumbHierarchy . BH.body
    currentTgt <- ASTRead.getCurrentASTTarget
    outLink    <- ASTRead.getFirstNonLambdaLink currentTgt
    case (,) <$> outLink <*> newOut of
        Just (l, o) -> IR.replaceSource o l
        Nothing     -> return ()
    IR.deepDeleteWithWhitelist oldSeq $ Set.fromList $ maybeToList newOut
    oldRef <- use $ Graph.breadcrumbHierarchy . BH.self
    when (oldRef == oldSeq) $ forM_ newOut (Graph.breadcrumbHierarchy . BH.self .=)
    forM_ newOut $ (Graph.breadcrumbHierarchy . BH.body .=)

updateCodeSpan' :: GraphOp m => NodeRef -> m _
updateCodeSpan' ref = IR.matchExpr ref $ \case
    IR.Seq l r -> do
        l' <- updateCodeSpan' =<< IR.source l
        r' <- updateCodeSpan' =<< IR.source r
        let span = l' <> r'
        setCodeSpan ref span
        return span
    _ -> readCodeSpan ref

updateCodeSpan :: GraphOp m => NodeRef -> m ()
updateCodeSpan ref = do
    updateCodeSpan' ref
    LeftSpacedSpan (SpacedSpan off len) <- readCodeSpan ref
    fileOffset <- use Graph.fileOffset
    setCodeSpan ref (leftSpacedSpan fileOffset len)

addPort :: GraphLocation -> OutPortRef -> Empire InputSidebar
addPort loc portRef = withTC loc False $ addPortNoTC loc portRef

addPortNoTC :: GraphLocation -> OutPortRef -> Command Graph InputSidebar
addPortNoTC loc (OutPortRef nl pid) = runASTOp $ do
    let nid      = nl ^. NodeLoc.nodeId
        position = getPortNumber pid
    (inE, _) <- GraphBuilder.getEdgePortMapping
    when (inE /= nid) $ throwM NotInputEdgeException
    ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    ASTModify.addLambdaArg position ref
    newLam <- ASTRead.getCurrentASTTarget
    ASTBuilder.attachNodeMarkersForArgs nid [] newLam
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
removeNodes loc@(GraphLocation file (Breadcrumb [])) nodeIds = do
    withUnit loc $ do
        funs <- use Graph.clsFuns
        let funsUUIDs = Map.fromList $ map (\(k, (n,g)) -> (k, n)) $ Map.assocs funs
            funsToRemove = catMaybes $ map (flip Map.lookup funsUUIDs) nodeIds

        let graphsToRemove = Map.elems $ Map.filterWithKey (\a _ -> a `elem` nodeIds) funs
        Graph.clsFuns .= Map.filterWithKey (\a _ -> a `notElem` nodeIds) funs

        klass <- use Graph.clsClass
        runASTOp $ IR.matchExpr klass $ \case
            IR.Unit _ _ cls -> do
                cls' <- IR.source cls
                Just (cls'' :: IR.Expr (IR.ClsASG)) <- IR.narrow cls'
                funs <- IR.matchExpr cls' $ \case
                    IR.ClsASG _ _ _ f -> do
                        links <- mapM (\link -> (link,) <$> IR.source link) f
                        forM links $ \(link, fun) -> do
                            IR.matchExpr fun $ \case
                                IR.ASGRootedFunction name _ -> return $ if convert name `elem` funsToRemove then Left link else Right link
                let (toRemove, left) = partitionEithers funs
                spans <- forM toRemove $ \candidate -> do
                    ref <- IR.source candidate
                    start <- Code.functionBlockStartRef ref
                    LeftSpacedSpan (SpacedSpan _ len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan.CodeSpan ref
                    return (start, start + len)
                forM (reverse spans) $ uncurry Code.removeAt
                IR.modifyExprTerm cls'' $ wrapped . IR.termClsASG_decls .~ (map IR.unsafeGeneralize left)
                mapM (IR.deleteSubtree <=< IR.source) toRemove
        return ()
removeNodes loc@(GraphLocation file _) nodeIds = do
    withTC loc False $ runASTOp $ mapM removeNodeNoTC nodeIds
    resendCode loc

deepRemoveExprMarkers :: GraphOp m => BH.BChild -> m ()
deepRemoveExprMarkers chld = do
    removeExprMarker $ chld ^. BH.self
    traverseOf_ (BH._LambdaChild . BH.children     . traverse) deepRemoveExprMarkers chld
    traverseOf_ (BH._ExprChild   . BH.portChildren . traverse . re BH._LambdaChild) deepRemoveExprMarkers chld

removeNodeNoTC :: GraphOp m => NodeId -> m [NodeId]
removeNodeNoTC nodeId = do
    astRef        <- ASTRead.getASTRef nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    mapM deepRemoveExprMarkers =<< use (Graph.breadcrumbHierarchy . BH.children . at nodeId)
    Graph.breadcrumbHierarchy . BH.children . at nodeId .= Nothing
    removeFromSequence astRef
    return $ map (view PortRef.dstNodeId) obsoleteEdges

removeExprMarker :: GraphOp m => NodeRef -> m ()
removeExprMarker ref = do
    exprMap <- getExprMap
    let newExprMap = Map.filter (/= ref) exprMap
    setExprMap newExprMap

removeSequenceElement :: GraphOp m => NodeRef -> NodeRef -> m (Maybe NodeRef, Bool)
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

removeFromSequence :: GraphOp m => NodeRef -> m ()
removeFromSequence ref = do
    oldSeq <- use $ Graph.breadcrumbHierarchy . BH.body
    (newS, shouldUpdate) <- removeSequenceElement oldSeq ref
    when shouldUpdate (updateGraphSeq newS)
    mapM_ Code.gossipLengthsChanged newS

removePort :: GraphLocation -> OutPortRef -> Empire InputSidebar
removePort loc portRef = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    ref <- ASTRead.getCurrentASTTarget
    ASTBuilder.detachNodeMarkersForArgs ref
    (inE, _) <- GraphBuilder.getEdgePortMapping
    newRef   <- if nodeId == inE then ASTModify.removeLambdaArg (portRef ^. PortRef.srcPortId) ref
                                 else throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] newRef
    GraphBuilder.buildInputSidebar nodeId

movePort :: GraphLocation -> OutPortRef -> Int -> Empire InputSidebar
movePort loc portRef newPosition = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    ref        <- ASTRead.getCurrentASTTarget
    (input, _) <- GraphBuilder.getEdgePortMapping
    newRef     <- if nodeId == input then ASTModify.moveLambdaArg (portRef ^. PortRef.srcPortId) newPosition ref
                                     else throwM NotInputEdgeException
    when (ref /= newRef) $ ASTModify.rewireCurrentNode newRef
    ASTBuilder.attachNodeMarkersForArgs nodeId [] ref
    GraphBuilder.buildInputSidebar nodeId

renamePort :: GraphLocation -> OutPortRef -> Text -> Empire InputSidebar
renamePort loc portRef newName = withGraph loc $ runASTOp $ do
    let nodeId = portRef ^. PortRef.srcNodeId
    ref        <- ASTRead.getCurrentASTTarget
    (input, _) <- GraphBuilder.getEdgePortMapping
    if nodeId == input then ASTModify.renameLambdaArg (portRef ^. PortRef.srcPortId) (Text.unpack newName) ref
                       else throwM NotInputEdgeException
    GraphBuilder.buildInputSidebar nodeId

setNodeExpression :: GraphLocation -> NodeId -> Text -> Empire ExpressionNode
setNodeExpression loc@(GraphLocation file _) nodeId expr' = do
    let expression = Text.strip expr'
    node <- withTC loc False $ do
        oldExpr   <- runASTOp $ ASTRead.getASTTarget nodeId
        parsedRef <- view _1 <$> ASTParse.runReparser expression oldExpr
        (oldBeg, oldEnd) <- runASTOp $ do
            oldBegin  <- Code.getASTTargetBeginning nodeId
            oldTarget <- ASTRead.getASTTarget nodeId
            oldLen    <- IR.getLayer @SpanLength oldTarget
            Code.propagateLengths parsedRef
            ASTModify.rewireNode nodeId parsedRef
            return (oldBegin, oldBegin + oldLen)
        runAliasAnalysis
        node <- runASTOp $ do
            expr      <- ASTRead.getASTTarget nodeId
            marked    <- ASTRead.getASTRef nodeId
            item      <- prepareChild (NodeCache Map.empty Map.empty Map.empty) marked parsedRef
            Graph.breadcrumbHierarchy . BH.children . ix nodeId .= item
            let len = fromIntegral $ Text.length expression
            Code.applyDiff oldBeg oldEnd expression
            Code.gossipUsesChangedBy (len - (oldEnd - oldBeg)) expr
            node <- GraphBuilder.buildNode nodeId
            return node
        return node
    resendCode loc
    return node

updateExprMap :: GraphOp m => NodeRef -> NodeRef -> m ()
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

setNodeMetaGraph :: NodeId -> NodeMeta -> Command Graph ()
setNodeMetaGraph nodeId newMeta = runASTOp $ do
    ref <- ASTRead.getASTRef nodeId
    AST.writeMeta ref newMeta

setNodeMetaFun :: NodeId -> NodeMeta -> Command ClsGraph ()
setNodeMetaFun nodeId newMeta = runASTOp $ do
    Just (name, _) <- use $ Graph.clsFuns . at nodeId
    f              <- ASTRead.getFunByName name
    AST.writeMeta f newMeta

setNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
setNodeMeta loc nodeId newMeta = withGraph' loc (setNodeMetaGraph nodeId newMeta) (setNodeMetaFun nodeId newMeta)

setNodePosition :: GraphLocation -> NodeId -> Position -> Empire ()
setNodePosition loc nodeId newPos = do
    oldMeta <- fromMaybe def <$> getNodeMeta loc nodeId
    setNodeMeta loc nodeId $ oldMeta & NodeMeta.position .~ newPos

setNodePositionAST :: GraphOp m => NodeId -> Position -> m ()
setNodePositionAST nodeId newPos = do
    ref <- ASTRead.getASTRef nodeId
    oldMeta <- fromMaybe def <$> AST.readMeta ref
    AST.writeMeta ref $ oldMeta & NodeMeta.position .~ newPos

setNodePositionCls :: ClassOp m => NodeId -> Position -> m ()
setNodePositionCls nodeId newPos = do
    Just (name, _) <- use $ Graph.clsFuns . at nodeId
    f              <- ASTRead.getFunByName name
    oldMeta <- fromMaybe def <$> AST.readMeta f
    AST.writeMeta f $ oldMeta & NodeMeta.position .~ newPos

connectCondTC :: Bool -> GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connectCondTC True  loc outPort anyPort = connect loc outPort anyPort
connectCondTC False loc outPort anyPort = do
    connection <- withGraph loc $ connectNoTC loc outPort anyPort
    resendCode loc
    return connection

connect :: GraphLocation -> OutPortRef -> AnyPortRef -> Empire Connection
connect loc outPort anyPort = do
    connection <- withTC loc False $ connectNoTC loc outPort anyPort
    resendCode loc
    return connection

connectPersistent :: GraphOp m => OutPortRef -> AnyPortRef -> m Connection
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
    withTC loc False $ runASTOp $ disconnectPort port
    resendCode loc

getNodeMeta :: GraphLocation -> NodeId -> Empire (Maybe NodeMeta)
getNodeMeta loc = withGraph loc . runASTOp . AST.getNodeMeta

getCode :: GraphLocation -> Empire String
getCode loc@(GraphLocation file _) = Text.unpack . Code.removeMarkers <$> withUnit (GraphLocation file (Breadcrumb [])) (use Graph.clsCode)

-- TODO[MK]: handle span
getBuffer :: FilePath -> Maybe (Int, Int) -> Empire Text
getBuffer file span = Text.pack <$> getCode (GraphLocation file (Breadcrumb []))

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withTC' loc True (runASTOp $ do
    exc <- use $ Graph.parseError
    case exc of
        Just e  -> throwM e
        Nothing -> GraphBuilder.buildGraph)
    (runASTOp $ do
        exc <- use $ Graph.clsParseError
        case exc of
            Just e  -> throwM e
            Nothing -> GraphBuilder.buildClassGraph)

getGraphNoTC :: GraphLocation -> Empire APIGraph.Graph
getGraphNoTC loc = withGraph' loc (runASTOp GraphBuilder.buildGraph) (runASTOp GraphBuilder.buildClassGraph)

getNodes :: GraphLocation -> Empire [ExpressionNode]
getNodes loc = withTC' loc True (runASTOp (view APIGraph.nodes <$> GraphBuilder.buildGraph))
                                (runASTOp (view APIGraph.nodes <$> GraphBuilder.buildClassGraph))

getConnections :: GraphLocation -> Empire [(OutPortRef, InPortRef)]
getConnections loc = withTC loc True $ runASTOp $ view APIGraph.connections <$> GraphBuilder.buildGraph

decodeLocation :: GraphLocation -> Empire (Breadcrumb (Named BreadcrumbItem))
decodeLocation loc@(GraphLocation file crumbs) = case crumbs of
        Breadcrumb [] -> return $ Breadcrumb []
        _             -> do
            definitionsIDs <- withUnit (GraphLocation file (Breadcrumb [])) $ do
                funs <- use Graph.clsFuns
                return $ Map.map fst funs
            withGraph (functionLocation loc) $ GraphBuilder.decodeBreadcrumbs definitionsIDs crumbs

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = do
    withTC loc False $ runASTOp $ do
        v <- ASTRead.getASTVar nid
        ASTModify.renameVar v $ convert name
        Code.replaceAllUses v name
    resendCode loc

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ return ()

autolayoutNodes :: GraphOp m => [NodeId] -> m ()
autolayoutNodes nids = do
    nodes <- GraphBuilder.buildNodes
    conns <- GraphBuilder.buildConnections
    let autolayout = Autolayout.autolayoutNodes nids nodes conns
    mapM_ (uncurry setNodePositionAST) autolayout

openFile :: FilePath -> Empire ()
openFile path = do
    code <- liftIO $ Text.readFile path
    Library.createLibrary Nothing path
    let loc = GraphLocation path $ Breadcrumb []
    result <- handle (\(e :: SomeASTException) -> return $ Left e) $ fmap Right $ do
        loadCode loc code
        withUnit loc $ Graph.clsParseError .= Nothing
    case result of
        Left e -> withUnit loc $ Graph.clsParseError ?= e
        _      -> return ()

typecheck :: GraphLocation -> Empire ()
typecheck loc = withTC' loc False (return ()) (return ())

substituteCodeFromPoints :: FilePath -> Point -> Point -> Text -> Maybe Point -> Empire ()
substituteCodeFromPoints path start end code cursor = do
    let loc = GraphLocation path (Breadcrumb [])
    (s, e) <- withUnit loc $ do
        oldCode   <- use Graph.code
        let noMarkers  = Code.removeMarkers oldCode
            deltas     = (Code.pointToDelta start noMarkers, Code.pointToDelta end noMarkers)
            realDeltas = Code.viewDeltasToReal oldCode deltas
        return realDeltas
    substituteCode path s e code Nothing

substituteCode :: FilePath -> Delta -> Delta -> Text -> Maybe Delta -> Empire ()
substituteCode path start end code cursor = do
    let loc = GraphLocation path (Breadcrumb [])
    newCode <- withUnit loc $ Code.applyDiff start end code
    handle (\(e :: SomeASTException) -> withUnit loc $ Graph.clsParseError ?= e) $ do
        withUnit loc $ Graph.clsParseError .= Nothing
        reloadCode loc newCode

lamItemToMapping :: ((NodeId, Maybe Int), BH.LamItem) -> ((NodeId, Maybe Int), (NodeId, NodeId))
lamItemToMapping (idArg, BH.LamItem portMapping _ _ _) = (idArg, portMapping)

extractMarkedMetasAndIds :: GraphOp m => NodeRef -> m [(Word64, (Maybe NodeMeta, Maybe NodeId))]
extractMarkedMetasAndIds root = IR.matchExpr root $ \case
    IR.Marked m e -> do
        meta   <- AST.readMeta root
        marker <- getMarker =<< IR.source m
        expr   <- IR.source e
        rest   <- extractMarkedMetasAndIds expr
        nid    <- ASTRead.getNodeId  expr
        return $ (marker, (meta, nid)) : rest
    _ -> concat <$> (mapM (extractMarkedMetasAndIds <=< IR.source) =<< IR.inputs root)

reloadCode :: GraphLocation -> Text -> Empire ()
reloadCode loc@(GraphLocation file _) code = do
    funs <- withUnit (GraphLocation file (Breadcrumb [])) $ do
        funs <- use Graph.clsFuns
        return $ Map.keys funs
    oldMetasAndIds <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        root       <- use $ Graph.breadcrumbHierarchy . BH.body
        oldMetas   <- extractMarkedMetasAndIds root
        return $ Map.fromList oldMetas
    previousPortMappings <- forM funs $ \fun -> withGraph (GraphLocation file (Breadcrumb [Breadcrumb.Definition fun])) $ runASTOp $ do
        hierarchy <- use Graph.breadcrumbHierarchy
        let lamItems = BH.getLamItems hierarchy
            elems    = map lamItemToMapping lamItems
        return $ Map.fromList elems
    let previousNodeIds   = Map.unions $ Map.mapMaybe snd <$> oldMetasAndIds
        previousNodeMetas = Map.unions $ Map.mapMaybe fst <$> oldMetasAndIds
    withUnit (GraphLocation file (Breadcrumb [])) $ Graph.nodeCache .= NodeCache previousNodeIds previousNodeMetas (Map.unions previousPortMappings)
    loadCode loc code

putIntoHierarchy :: GraphOp m => NodeId -> NodeRef -> m ()
putIntoHierarchy nodeId marked = do
    let nodeItem = BH.ExprItem Map.empty marked
    Graph.breadcrumbHierarchy . BH.children . at nodeId ?= BH.ExprChild nodeItem

putChildrenIntoHierarchy :: GraphOp m => NodeId -> NodeRef -> m ()
putChildrenIntoHierarchy uuid expr = do
    target       <- ASTRead.getASTTarget uuid
    marked       <- ASTRead.getASTRef uuid
    item         <- prepareChild (NodeCache Map.empty Map.empty Map.empty) marked target
    Graph.breadcrumbHierarchy . BH.children . ix uuid .= item

copyMeta :: GraphOp m => NodeRef -> NodeRef -> m ()
copyMeta donor recipient = do
    meta <- AST.readMeta donor
    forM_ meta $ AST.writeMeta recipient

markNode :: GraphOp m => NodeId -> m ()
markNode nodeId = do
    var <- ASTRead.getASTMarkerPosition nodeId
    ASTBuilder.attachNodeMarkers nodeId [] var

loadCode :: GraphLocation -> Text -> Empire ()
loadCode loc@(GraphLocation file _) code = do
    (unit, IR.Rooted ir ref, exprMap) <- liftIO $ ASTParse.runProperParser code
    activeFiles . at file . traverse . Library.body . Graph.clsClass .= unit
    activeFiles . at file . traverse . Library.body . Graph.clsCodeMarkers .= (coerce exprMap)
    activeFiles . at file . traverse . Library.body . Graph.code .= code
    funs <- use $ activeFiles . at file . traverse . Library.body . Graph.clsFuns
    let funsUUIDs = Map.fromList $ map (\(k, (n,g)) -> (n, k)) $ Map.assocs funs
    activeFiles . at file . traverse . Library.body . Graph.clsFuns .= Map.empty
    withUnit (GraphLocation file (Breadcrumb [])) $ putNewIRCls ir
    functions <- withUnit loc $ do
        klass <- use Graph.clsClass
        runASTOp $ do
            funs <- AST.classFunctions klass
            forM funs $ \f -> IR.matchExpr f $ \case
                IR.ASGRootedFunction name _ -> return (convert name, f)
    forM_ functions $ \(name, fun) -> do
        let lastUUID = Map.lookup name funsUUIDs
        uuid <- Library.withLibrary file (fst <$> makeGraph fun lastUUID)
        let loc' = GraphLocation file $ Breadcrumb [Breadcrumb.Definition uuid]
        autolayout loc'
    autolayoutTopLevel loc
    return ()

infixl 5 |>
(|>) :: GraphLocation -> BreadcrumbItem -> GraphLocation
(|>) (GraphLocation file bc) item = GraphLocation file $ coerce $ (++ [item]) $ coerce bc

autolayout :: GraphLocation -> Empire ()
autolayout loc = do
    kids <- withGraph loc $ runASTOp $ do
        kids <- uses Graph.breadcrumbHierarchy (view BH.children)
        needLayout <- fmap catMaybes $ forM (Map.keys kids) $ \id -> do
            meta <- AST.getNodeMeta id
            return $ if meta /= def then Nothing else Just id
        autolayoutNodes needLayout
        return kids
    let next = concatMap (\(k, v) -> case v of
            BH.LambdaChild{}                -> [Breadcrumb.Lambda k]
            BH.ExprChild (BH.ExprItem pc _) -> map (Breadcrumb.Arg k) (Map.keys pc)) $ Map.assocs kids
    mapM_ (\a -> autolayout (loc |> a)) next

autolayoutTopLevel :: GraphLocation -> Empire ()
autolayoutTopLevel loc = do
    withUnit loc $ runASTOp $ do
        clsFuns <- use Graph.clsFuns
        needLayout <- fmap catMaybes $ forM (Map.assocs clsFuns) $ \(id, (name, _)) -> do
            f    <- ASTRead.getFunByName name
            meta <- AST.readMeta f
            return $ if meta /= def then Nothing else Just id

        nodes <- view APIGraph.nodes <$> GraphBuilder.buildClassGraph
        let autolayout = Autolayout.autolayoutNodes needLayout nodes []
        mapM_ (uncurry setNodePositionCls) autolayout

printMarkedExpression :: GraphOp m => NodeRef -> m Text
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

isSidebar :: GraphOp m => NodeId -> m Sidebar
isSidebar nodeId = do
    (input, output) <- GraphBuilder.getEdgePortMapping
    return $ if | input  == nodeId -> SidebarInput
                | output == nodeId -> SidebarOutput
                | otherwise        -> NotSidebar

isInput :: GraphOp m => NodeId -> m Bool
isInput nodeId = (== SidebarInput) <$> isSidebar nodeId

isOutput :: GraphOp m => NodeId -> m Bool
isOutput nodeId = (== SidebarOutput) <$> isSidebar nodeId

functionLocation :: GraphLocation -> GraphLocation
functionLocation (GraphLocation file (Breadcrumb b))
    | ((Breadcrumb.Definition f) : _) <- b = GraphLocation file (Breadcrumb [Breadcrumb.Definition f])
    | otherwise = GraphLocation file (Breadcrumb [])

previousOffset :: GraphOp m => NodeRef -> m Delta
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

readRangeProper :: GraphOp m => NodeRef -> m (LeftSpacedSpan Delta)
readRangeProper ref = do
    refSpan@(LeftSpacedSpan (SpacedSpan off len)) <- readCodeSpan ref
    moreOffset <- previousOffset ref
    let properOffset = off + moreOffset
        properSpan   = leftSpacedSpan properOffset len
    return properSpan

readRange :: GraphOp m => NodeRef -> m (Int, Int)
readRange ref = do
    LeftSpacedSpan (SpacedSpan offset len) <- readRangeProper ref
    fileOffset <- fromMaybe 0 <$> Code.getOffsetRelativeToFile ref
    return (fromIntegral (fileOffset), fromIntegral (fileOffset + len))

readCodeSpan :: GraphOp m => NodeRef -> m (LeftSpacedSpan Delta)
readCodeSpan ref = view CodeSpan.realSpan <$> IR.getLayer @CodeSpan ref

setCodeSpan :: GraphOp m => NodeRef -> LeftSpacedSpan Delta -> m ()
setCodeSpan ref s = IR.putLayer @CodeSpan ref $ CodeSpan.mkRealSpan s

getNodeIdForMarker :: GraphOp m => Int -> m (Maybe NodeId)
getNodeIdForMarker index = do
    exprMap      <- getExprMap
    let exprMap' :: Map.Map Luna.MarkerId NodeRef
        exprMap' = coerce exprMap
        Just ref = Map.lookup (fromIntegral index) exprMap'
    IR.matchExpr ref $ \case
        IR.Marked _m expr -> do
            expr'     <- IR.source expr
            nodeId    <- ASTRead.getNodeId expr'
            return nodeId

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

printNodeLine :: GraphOp m => NodeId -> m String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= ASTPrint.printExpression

withTC' :: GraphLocation -> Bool -> Command Graph a -> Command ClsGraph a -> Empire a
withTC' loc@(GraphLocation file bs) flush actG actC = do
    res <- withGraph' loc actG actC
    case bs of
        Breadcrumb []      -> return ()
        Breadcrumb (d : _) -> withGraph' (GraphLocation file (Breadcrumb [d])) (runTC loc flush) (return ())
    return res

withTCUnit :: GraphLocation -> Bool -> Command ClsGraph a -> Empire a
withTCUnit loc flush cmd = withTC' loc flush (throwM UnsupportedOperation) cmd

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc flush actG = withTC' loc flush actG (throwM UnsupportedOperation)

data UnsupportedOperation = UnsupportedOperation
    deriving Show

instance Exception UnsupportedOperation where
    fromException = astExceptionFromException
    toException = astExceptionToException

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation file breadcrumb) act = withBreadcrumb file breadcrumb act (throwM UnsupportedOperation)

withUnit :: GraphLocation -> Command ClsGraph a -> Empire a
withUnit (GraphLocation file breadcrumb) act = withBreadcrumb file breadcrumb (throwM UnsupportedOperation) act

withGraph' :: GraphLocation -> Command Graph a -> Command ClsGraph a -> Empire a
withGraph' (GraphLocation file breadcrumb) actG actC = withBreadcrumb file breadcrumb actG actC

getOutEdges :: GraphOp m => NodeId -> m [InPortRef]
getOutEdges nodeId = do
    edges <- GraphBuilder.buildConnections
    let filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: GraphOp m => InPortRef -> m ()
disconnectPort (InPortRef (NodeLoc _ dstNodeId) dstPort) = case dstPort of
    []        -> setToNothing dstNodeId
    [Self]    -> unAcc dstNodeId
    [Arg num] -> unApp dstNodeId num

setToNothing :: GraphOp m => NodeId -> m ()
setToNothing dst = do
    (_, out) <- GraphBuilder.getEdgePortMapping
    let disconnectOutputEdge = out == dst
        nothingExpr          = "None"
    nothing <- IR.generalize <$> IR.cons_ (convert nothingExpr)
    IR.putLayer @SpanLength nothing $ convert $ Text.length nothingExpr
    if disconnectOutputEdge
        then setOutputTo nothing
        else GraphUtils.rewireNode dst nothing

unAcc :: GraphOp m => NodeId -> m ()
unAcc nodeId = do
    dstAst     <- ASTRead.getTargetEdge nodeId
    beg        <- Code.getASTTargetBeginning nodeId
    ASTBuilder.removeAccessor dstAst beg

unApp :: GraphOp m => NodeId -> Int -> m ()
unApp nodeId pos = do
    dstAst <- ASTRead.getTargetEdge nodeId
    beg    <- Code.getASTTargetBeginning nodeId
    ASTBuilder.removeArgument dstAst beg pos

makeAcc :: GraphOp m => NodeId -> NodeId -> OutPortId -> m ()
makeAcc src dst outPort = do
    dstBeg     <- Code.getASTTargetBeginning dst
    srcAst     <- ASTRead.getASTOutForPort src outPort
    dstAst     <- ASTRead.getTargetEdge dst
    ASTBuilder.makeAccessor srcAst dstAst dstBeg

makeApp :: GraphOp m => NodeId -> NodeId -> Int -> OutPortId -> m ()
makeApp src dst pos outPort = do
    dstBeg     <- Code.getASTTargetBeginning dst
    srcAst     <- ASTRead.getASTOutForPort src outPort
    dstAst     <- ASTRead.getTargetEdge dst
    ASTBuilder.applyFunction dstAst dstBeg srcAst pos

makeWhole :: GraphOp m => NodeId -> NodeId -> OutPortId -> m ()
makeWhole src dst outPort = do
    (_, out) <- GraphBuilder.getEdgePortMapping
    let connectToOutputEdge = out == dst
    srcAst   <- ASTRead.getASTOutForPort src outPort
    if connectToOutputEdge then setOutputTo srcAst else GraphUtils.rewireNode dst srcAst
