{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module FileLoadSpec (spec) where

import           Control.Monad                   (forM)
import           Data.Coerce
import           Data.List                       (find)
import qualified Data.Map                        as Map
import           Data.Reflection                 (Given (..), give)
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import           Data.Text.Span                  (LeftSpacedSpan (..), SpacedSpan (..))
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.ASTOps.Parse             as ASTParse
import qualified Empire.ASTOps.Print             as ASTPrint
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.Library         as Library
import           Empire.Data.AST                 (SomeASTException)
import qualified Empire.Data.Graph               as Graph (code, codeMarkers, breadcrumbHierarchy)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Empire                   (CommunicationEnv (..), Empire)
import qualified Luna.Syntax.Text.Parser.Parser  as Parser (ReparsingChange (..), ReparsingStatus (..))
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Point           (Point (Point))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))

import           Empire.Prelude
import           Luna.Prelude                    (normalizeQQ)

import           Test.Hspec                      (Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                                  shouldNotBe, shouldSatisfy, shouldStartWith, xit, Expectation)

import           EmpireUtils

import           Text.RawString.QQ               (r)

import qualified Luna.IR                         as IR

mainCondensed = [r|def main:
    «0»pi = 3.14
    «1»foo = a: b: a + b
    «2»c = 4
    «3»bar = foo 8 c
|]

mainFile = [r|def main:
    «0»pi = 3.14

    «1»foo = a: b: a + b

    «2»c = 4
    «3»bar = foo 8 c
|]

testLuna = [r|def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «5»lala = 17.0
        «12»buzz = x: y:
            «9»x * y
        «6»pi = 3.14
        «7»n = buzz a lala
        «8»m = buzz b pi
        «11»m + n
    «2»c = 4.0
    «3»bar = foo 8.0 c
|]

atXPos = ($ def) . (NodeMeta.position . Position.x .~)

specifyCodeChange :: Text -> Text -> (GraphLocation -> Empire a) -> CommunicationEnv -> Expectation
specifyCodeChange initialCode expectedCode act env = do
    let normalize = Text.pack . normalizeQQ . Text.unpack
    actualCode <- evalEmp env $ do
        Library.createLibrary Nothing "TestPath" $ normalize initialCode
        let loc = GraphLocation "TestPath" $ Breadcrumb []
        Graph.withGraph loc $ Graph.loadCode $ normalize initialCode
        (nodeIds, toplevel) <- Graph.withGraph loc $ do
            markers  <- fmap fromIntegral . Map.keys <$> use Graph.codeMarkers
            ids      <- runASTOp $ forM markers $ \i -> (i,) <$> Graph.getNodeIdForMarker i
            toplevel <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
            return (ids, toplevel)
        forM nodeIds $ \(i, Just nodeId) ->
            when (elem nodeId toplevel) $
                Graph.setNodeMeta loc nodeId $ NodeMeta (Position.fromTuple (0, fromIntegral i*10)) False def
        act loc
        Graph.withGraph loc $ use Graph.code
    Text.strip actualCode `shouldBe` normalize expectedCode


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "text coordinates translation" $ do
        it "translates points to deltas and back" $ \env -> do
            let code = Text.unlines [ "  "
                                    , "foo   "
                                    , " barbaz"
                                    , ""
                                    , "cwddd   "
                                    , ""
                                    , ""
                                    ]
            Code.pointToDelta (Point 3 2) code `shouldBe` 13
            Code.pointToDelta (Point 0 0) code `shouldBe` 0
            Code.pointToDelta (Point 4 4) code `shouldBe` 23

            Code.deltaToPoint 13 code `shouldBe` (Point 3 2)
            Code.deltaToPoint 0  code `shouldBe` (Point 0 0)
            Code.deltaToPoint 23 code `shouldBe` (Point 4 4)
    describe "code marker removal" $ do
        it "removes markers" $ \env -> do
            let code = Text.unlines [ "def main:"
                                    , "    «0»foo = bar"
                                    , ""
                                    , "    «1»a = x: y: «2»bar + baz"
                                    , "    «3»foobar = bar * baz + foo"
                                    , "    "
                                    ]
                expectedCode = Text.unlines [ "def main:"
                                            , "    foo = bar"
                                            , ""
                                            , "    a = x: y: bar + baz"
                                            , "    foobar = bar * baz + foo"
                                            , "    "
                                            ]
            Code.removeMarkers code `shouldBe` expectedCode
    describe "file loading" $ do
        it "parses unit" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: a + b
                    «2»bar = foo c 6
                    «3»print pi
                    «4»c = 3
                |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                graph <- Graph.withGraph loc $ runASTOp $ GraphBuilder.buildGraph
                return graph
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                let Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                pi ^. Node.code `shouldBe` "3.14"
                pi ^. Node.canEnter `shouldBe` False
                let Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                foo ^. Node.code `shouldBe` "a: b: «5»a + b"
                foo ^. Node.canEnter `shouldBe` True
                let Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                bar ^. Node.code `shouldBe` "foo c 6"
                bar ^. Node.canEnter `shouldBe` False
                let Just anon = find (\node -> node ^. Node.name == Nothing) nodes
                anon ^. Node.code `shouldBe` "print pi"
                anon ^. Node.canEnter `shouldBe` False
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                c ^. Node.code `shouldBe` "3"
                c ^. Node.canEnter `shouldBe` False
                connections `shouldMatchList` [
                      (outPortRef (pi ^. Node.nodeId) [], inPortRef (anon ^. Node.nodeId) [Port.Arg 0])
                    ]
        it "does not duplicate nodes on edit" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainFile
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainFile
                Graph.substituteCode "TestPath" 43 43 "3" (Just 44)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "double modification gives proper value" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainFile
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainFile
                Graph.substituteCode "TestPath" 68 68 "3" (Just 66)
                Graph.substituteCode "TestPath" 68 68 "3" (Just 66)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                cNode ^. Node.code `shouldBe` "334"
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "modifying two expressions give proper values" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainFile
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainFile
                Graph.substituteCode "TestPath" 68 68 "3" (Just 66)
                Graph.substituteCode "TestPath" 88 88 "1" (Just 86)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                cNode ^. Node.code `shouldBe` "34"
                bar ^. Node.code `shouldBe` "foo 18 c"
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "adding an expression works" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainCondensed
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainCondensed
                Graph.substituteCode "TestPath" 89 89 "    «4»d = 10\n" (Just 86)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    Just d = find (\node -> node ^. Node.name == Just "d") nodes
                d ^. Node.code `shouldBe` "10"
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                connections `shouldMatchList` [
                      (outPortRef (c ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "unparseable expression does not sabotage whole file" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainCondensed
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainCondensed
                Graph.substituteCode "TestPath" 22 26 ")" (Just 26) `catch` (\(_e :: SomeASTException) -> return ())
                Graph.substituteCode "TestPath" 22 23 "5" (Just 23)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                    Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                pi ^. Node.code `shouldBe` "5"
                c ^. Node.code `shouldBe` "4"
                bar ^. Node.code `shouldBe` "foo 8 c"
                connections `shouldMatchList` [
                      (outPortRef (c ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "enters lambda written in file" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»foo = a: b: a + b
                    |]
                loc = GraphLocation "TestPath" $ Breadcrumb []
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.withGraph (loc |> foo) $ runASTOp $ GraphBuilder.buildGraph
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                nodes `shouldSatisfy` ((== 1) . length)
                connections `shouldSatisfy` ((== 3) . length)
        it "lambda in code can be entered" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»foo = a: a
                    |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.getGraph $ loc |> foo
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                nodes `shouldBe` []
                connections `shouldSatisfy` (not . null)
        it "autolayouts nodes on file load" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainCondensed
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainCondensed
                Graph.autolayout loc
                view Graph.nodes <$> Graph.getGraph loc
            let positions = map (view (Node.nodeMeta . NodeMeta.position)) nodes
                uniquePositions = Set.size $ Set.fromList positions
            uniquePositions `shouldBe` length nodes
        it "retains node ids on code reload" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" testLuna
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Just foo <- Graph.withGraph loc $ Graph.loadCode testLuna >> runASTOp (Graph.getNodeIdForMarker 1)
                Graph.substituteCode "TestPath" 65 66 "5" (Just 66)
                Graph.getNodes (loc |> foo)
            let Just lala = find (\n -> n ^. Node.name == Just "lala") nodes
            lala ^. Node.code `shouldBe` "15.0"
    describe "code spans" $ do
        it "simple example" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 5
                    |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                forM [0..0] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 23)
                    ]
        it "not so simple example" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 5
                        «1»a = 60
                    |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                forM [0..1] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 23)
                    , (28, 37)
                    ]
        it "shows proper expressions ranges" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainCondensed
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainCondensed
                forM [0..3] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 26)
                    , (31, 51)
                    , (56, 64)
                    , (69, 85)
                    ]
        it "updateCodeSpan does not break anything" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainCondensed
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ do
                    Graph.loadCode mainCondensed
                    runASTOp $ do
                        Just nodeSeq <- GraphBuilder.getNodeSeq
                        Graph.updateCodeSpan nodeSeq
                forM [0..3] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 26)
                    , (31, 51)
                    , (56, 64)
                    , (69, 85)
                    ]
        it "assigns nodeids to marked expressions" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" mainCondensed
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode mainCondensed
                Graph.withGraph loc $ runASTOp $ forM [0..3] Graph.getNodeIdForMarker
            withResult res $ \ids -> do
                ids `shouldSatisfy` (all isJust)
        it "autolayouts nested nodes on file load" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 3.14
                        «1»foo = a: b:
                            «5»lala = 17.0
                            «12»buzz = x: y:
                                «9»x * y
                            «6»pi = 3.14
                            «7»n = buzz a lala
                            «8»m = buzz b pi
                            «11»m + n
                    |]
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.autolayout loc
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                view Graph.nodes <$> Graph.getGraph (loc |> foo)
            let positions = map (view (Node.nodeMeta . NodeMeta.position)) nodes
                uniquePositions = Set.size $ Set.fromList positions
            uniquePositions `shouldBe` length nodes
    describe "code modifications by graph operations" $ do
        it "adds one node to code" $ \env -> do
            u1 <- mkUUID
            code <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.withGraph top $ use Graph.code
            code `shouldBe` "def main:\n    «0»node1 = 4\n"
        it "adds one node and updates it" $ \env -> do
            u1 <- mkUUID
            code <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.markerCodeSpan top 0
                Graph.setNodeExpression top u1 "5"
                Graph.withGraph top $ use Graph.code
            code `shouldBe` "def main:\n    «0»node1 = 5\n"
        xit "disconnect updates code at proper range" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                [Just c, Just bar] <- Graph.withGraph loc $ runASTOp $ mapM (Graph.getNodeIdForMarker) [2,3]
                Graph.disconnect loc (inPortRef bar [Port.Arg 1])
        {-it "disconnect/connect updates code at proper range" $ let-}
            {-expectedCode = [r|-}
                {-def main:-}
                    {-«0»pi = 3.14-}
                    {-«1»foo = a: b: a + b-}
                    {-«2»c = 4-}
                    {-«3»bar = foo 8 pi-}
                {-|]-}
            {-in specifyCodeChange mainCondensed expectedCode $ \loc -> do-}
                {-[Just pi, Just bar] <- Graph.withGraph loc $ runASTOp $ mapM (Graph.getNodeIdForMarker) [0,3]-}
                {-Graph.disconnect loc (inPortRef bar [Port.Arg 1])-}
                {-Graph.connect loc (outPortRef pi []) (InPortRef' $ inPortRef bar [Port.Arg 1])-}
        it "adds one node to existing file via node editor" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «5»node1 = 4
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \top -> do
                u1 <- mkUUID
                Graph.addNode top u1 "4" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "adds one node to the beginning of the file via node editor" $ let
            expectedCode = [r|
                def main:
                    «5»node1 = 4
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \top -> do
                u1 <- mkUUID
                Graph.addNode top u1 "4" (NodeMeta (Position.fromTuple (-10, 0)) False def)
        it "adds one named node to existing file via node editor" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «5»someNode = 123456789
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "someNode = 123456789" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "trims whitespace when adding node via node editor" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «5»node1 = 1
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "           1" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "preserves original whitespace inside expression when adding node" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «5»node1 = a:   b:   «6»a   *  b
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "a:   b:   a   *  b" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "adds lambda to existing file via node editor" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                    «5»node1 = x: x
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "x: x" (NodeMeta (Position.fromTuple (10, 50)) False def)
        it "adds node via node editor and removes it" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "x :   x" (NodeMeta (Position.fromTuple (10, 25)) False def)
                Graph.removeNodes loc [u1]
        it "removes last node form a file" $ let
            initialCode = [r|
                def main:
                    «0»pi = 3.14
                |]
            expectedCode = "def main:"
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just id <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.removeNodes loc [id]
        it "removes all nodes from a file, then adds some" $ let
            expectedCode = [r|
                def main:
                    «0»foo = 3 + 5
                    «1»bar = 20 + 30
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                ids <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..3]
                Graph.removeNodes loc (fromJust <$> ids)
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "foo = 3 + 5"   (atXPos 20.0)
                Graph.addNode loc u1 "bar = 20 + 30" (atXPos 30.0)

        it "adds and removes nodes inside a lambda" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «6»d = 8
                                   «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just id <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |> id
                u1 <- mkUUID
                Graph.addNode loc' u1 "a = 2 + 3 +    5" (atXPos 0)
                u2 <- mkUUID
                Graph.addNode loc' u2 "d = 8" (atXPos 10.0)
                Graph.removeNodes loc' [u1]
        it "updates code span after editing an expression" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 123456789
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1     <- mkUUID
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.setNodeExpression loc c "123456789"
        it "renames unused node in code" $ let
            expectedCode = [r|
                def main:
                    «0»ddd = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc pi "ddd"
        it "renames used node in code" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»ddd = 4
                    «3»bar = foo 8 ddd
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.renameNode loc c "ddd"
        it "adds one node to existing file and updates it" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                    «5»node1 = 5
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "4" (NodeMeta (Position.fromTuple (10, 50)) False def)
                Graph.setNodeExpression loc u1 "5"
        it "adds multiple nodes" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                    «6»node2 = add here
                    «5»node1 = (foo +  baz)
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "(foo +  baz)" (NodeMeta (Position.fromTuple (10, 60)) False def)
                u2 <- mkUUID
                Graph.addNode loc u1 "add here" (NodeMeta (Position.fromTuple (10, 50)) False def)
        it "combines adding and renaming nodes" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»b = a .    succ
                |]
            expectedCode = [r|
                def main:
                    «0»foobar = 20
                    «1»b = foobar .    succ
                    «2»bar = foobar .   div   foobar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just a <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc a "foo"
                u1 <- mkUUID
                Graph.addNode loc u1 "bar = foo .   div   foo" (NodeMeta (Position.fromTuple (0, 30)) False def)
                Graph.renameNode loc a "foobar"
        it "renames node under operator" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»b =   a + a
                |]
            expectedCode = [r|
                def main:
                    «0»foobar = 20
                    «1»b =   foobar + foobar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just a <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc a "foobar"
        it "connects self port retaining formatting" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»foo = 30
                    «2»b =   bar
                |]
            expectedCode = [r|
                def main:
                    «0»a = 20
                    «1»foo = 30
                    «2»b =   foo . bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just b, Just foo] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [2, 1]
                Graph.connect loc (outPortRef foo []) (InPortRef' $ inPortRef b [Port.Self])
        it "reconnects self port retaining formatting" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»foo = 30
                    «2»b = a  .   bar
                |]
            expectedCode = [r|
                def main:
                    «0»a = 20
                    «1»foo = 30
                    «2»b = foo  .   bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just b, Just foo] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [2, 1]
                Graph.connect loc (outPortRef foo []) (InPortRef' $ inPortRef b [Port.Self])
        it "disconnects self port retaining formatting" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»b =   a  .   bar
                |]
            expectedCode = [r|
                def main:
                    «0»a = 20
                    «1»b =   bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [1]
                Graph.disconnect loc (inPortRef b [Port.Self])
        it "connects and then disconnects self multiple times" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ
                |]
            expectedCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Self])
                Graph.disconnect loc (inPortRef b [Port.Self])
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Self])
                Graph.disconnect loc (inPortRef b [Port.Self])
        it "connects to application port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ baz
                |]
            expectedCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ baz _ node1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Arg 2])
        it "connects to application port multiple times" $ let
            initialCode = [r|
                def main:
                    «0»a  = foo
                    «1»bb = bar
                    «2»ccc = baz
                    «3»dddd = spam bb
                |]
            expectedCode = [r|
                def main:
                    «0»a  = foo
                    «1»bb = bar
                    «2»ccc = baz
                    «3»dddd = spam a bb ccc
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just bb, Just ccc, Just dddd] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..3]
                Graph.connect loc (outPortRef ccc []) (InPortRef' $ inPortRef dddd [Port.Arg 2])
                Graph.connect loc (outPortRef a   []) (InPortRef' $ inPortRef dddd [Port.Arg 0])
                Graph.connect loc (outPortRef bb  []) (InPortRef' $ inPortRef dddd [Port.Arg 1])

        xit "properly applies operators" $ let
            initialCode = [r|
                def main:
                    «0»aa = foo
                    «1»b  = +
                |]
            expectedCode = [r|
                def main:
                    «0»aa = foo
                    «1»b  = aa +
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just aa, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..1]
                Graph.connect loc (outPortRef aa []) (InPortRef' $ inPortRef b [Port.Arg 0])



        xit "updates code after disconnecting lambda output" $ let
            expectedCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: a + b
                            None
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Just (_, output) <- Graph.withGraph (loc |> foo) $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.disconnect (loc |> foo) (inPortRef output [])
