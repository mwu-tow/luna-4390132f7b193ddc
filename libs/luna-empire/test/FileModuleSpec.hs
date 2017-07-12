{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

module FileModuleSpec (spec) where

import           Data.List                      (find)
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import           Empire.ASTOp                   (runASTOp)
import qualified Empire.Commands.AST            as AST
import qualified Empire.Commands.Code           as Code
import qualified Empire.Commands.Graph          as Graph
import qualified Empire.Commands.GraphBuilder   as GraphBuilder
import qualified Empire.Commands.Library        as Library
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified Empire.Data.Graph              as Graph
import           LunaStudio.Data.Breadcrumb     (Breadcrumb (..), BreadcrumbItem (..))
import qualified LunaStudio.Data.Breadcrumb     as Breadcrumb
import qualified LunaStudio.Data.Graph          as Graph
import           LunaStudio.Data.GraphLocation  (GraphLocation (..))
import qualified LunaStudio.Data.Node           as Node
import           LunaStudio.Data.NodeMeta       (NodeMeta(..))
import qualified LunaStudio.Data.NodeMeta       as NodeMeta
import qualified LunaStudio.Data.Position       as Position

import           Luna.Prelude                   (forM, normalizeQQ)
import           Empire.Empire
import           Empire.Prelude

import           Test.Hspec                     (Expectation, Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                                 shouldNotBe, shouldSatisfy, shouldStartWith, xit)

import           EmpireUtils

import           Text.RawString.QQ              (r)


multiFunCode = [r|def foo:
    5

def bar:
    "bar"

def main:
    print bar
|]

codeWithImport = [r|import Std

def foo:
    5

def bar:
    "bar"

def main:
    print bar
|]

atXPos = ($ def) . (NodeMeta.position . Position.x .~)

specifyCodeChange :: Text -> Text -> (GraphLocation -> Empire a) -> CommunicationEnv -> Expectation
specifyCodeChange initialCode expectedCode act env = do
    let normalize = Text.pack . normalizeQQ . Text.unpack
    actualCode <- evalEmp env $ do
        Library.createLibrary Nothing "TestPath"
        let loc = GraphLocation "TestPath" $ Breadcrumb []
        Graph.loadCode loc $ normalize initialCode
        [main] <- filter (\n -> n ^. Node.name == Just "main") <$> Graph.getNodes loc
        let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
        act loc'
        Text.pack <$> Graph.getCode loc'
    Text.strip actualCode `shouldBe` normalize expectedCode


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "multi-module files" $ do
        it "shows functions at file top-level" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                return nodes
            length nodes `shouldBe` 3
            -- nodes are layouted in a left-to-right manner
            let uniquePositions = Set.toList $ Set.fromList $ map (view (Node.nodeMeta . NodeMeta.position . Position.x)) nodes
            length uniquePositions `shouldBe` 3
        it "adds function at top-level" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "quux" def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ code `shouldBe` normalizeQQ [r|
                def quux:
                    None

                def foo:
                    5

                def bar:
                    "bar"

                def main:
                    print bar
                |]
        it "adds function at top-level as third function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "quux" $ set NodeMeta.position (Position.fromTuple (200,0)) def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ code `shouldBe` normalizeQQ [r|
                def foo:
                    5

                def bar:
                    "bar"

                def quux:
                    None

                def main:
                    print bar
                |]
        it "adds function at top-level as last function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "quux" $ set NodeMeta.position (Position.fromTuple (500,0)) def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ code `shouldBe` normalizeQQ [r|
                def foo:
                    5

                def bar:
                    "bar"

                def main:
                    print bar

                def quux:
                    None
                |]
        it "adds function at top-level as def" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "def quux" def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ code `shouldBe` normalizeQQ [r|
                def quux:
                    None

                def foo:
                    5

                def bar:
                    "bar"

                def main:
                    print bar
                |]
        it "enters just added function" $ \env -> do
            u1 <- mkUUID
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                n <- Graph.addNode loc u1 "quux" def
                Graph.getNodes (GraphLocation "TestPath" (Breadcrumb [Definition (n ^. Node.nodeId)]))
            length nodes `shouldBe` 0
        it "removes function at top-level" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [bar ^. Node.nodeId]
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isNothing
            normalizeQQ code `shouldBe` normalizeQQ [r|
                def foo:
                    5

                def main:
                    print bar
                |]
        it "adds and removes function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                n <- Graph.addNode loc u1 "def quux" def
                Graph.removeNodes loc [n ^. Node.nodeId]
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isNothing
            normalizeQQ code `shouldBe` normalizeQQ multiFunCode
        it "decodes breadcrumbs in function" $ \env -> do
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
            Breadcrumb location <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                nodes <- Graph.getNodes loc'
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                let loc'' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId), Lambda (foo ^. Node.nodeId)]
                Graph.decodeLocation loc''
            let names = map (view Breadcrumb.name) location
            names `shouldBe` ["main", "foo"]
        it "decodes breadcrumbs in function 2" $ \env -> do
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
            Breadcrumb location <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                nodes <- Graph.getNodes loc'
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                let loc'' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId), Lambda (foo ^. Node.nodeId)]
                nodes <- Graph.getNodes loc''
                let Just buzz = find (\n -> n ^. Node.name == Just "buzz") nodes
                let loc''' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId), Lambda (foo ^. Node.nodeId), Lambda (buzz ^. Node.nodeId)]
                Graph.decodeLocation loc'''
            let names = map (view Breadcrumb.name) location
            names `shouldBe` ["main", "foo", "buzz"]
        it "does not crash on substitute with multiple functions" $ \env -> do
            evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.substituteCode "TestPath" 13 14 "10" (Just 14)
        it "shows proper function offsets without imports" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                Graph.withUnit loc $ runASTOp $ forM funIds $ Code.functionBlockStart
            sort offsets `shouldBe` [0, 19, 42]
        it "shows proper function offsets with imports" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithImport
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                Graph.withUnit loc $ runASTOp $ forM funIds $ Code.functionBlockStart
            sort offsets `shouldBe` [12, 31, 54]
        it "adds node in a function with at least two nodes" $
            let initialCode = [r|
                    def foo:
                        «0»5
                    def bar:
                        «1»"bar"
                    def main:
                        «2»c = 4
                        «3»print bar
                    |]
                expectedCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        node1 = 5
                        c = 4
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "5" (atXPos (-50))
        it "adds node in a function with at least two nodes in a file without markers" $
            let initialCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        c = 4
                        print bar
                    |]
                expectedCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        node1 = 5
                        c = 4
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "5" (atXPos (-50))
        it "adds node in a function with one node" $
            let initialCode = [r|
                    def foo:
                        «0»5
                    def bar:
                        «1»"bar"
                    def main:
                        «2»print bar
                    |]
                expectedCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        node1 = 5
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "5" (atXPos (-50))
        xit "adds node in two functions" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                u1 <- mkUUID
                Graph.addNode (loc |>= foo) u1 "5" (atXPos (-10))
                let Just main = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "main") nodes
                u2 <- mkUUID
                Graph.addNode (loc |>= main) u2 "1" (atXPos (-10))
                Graph.getCode loc
            normalizeQQ code `shouldBe` normalizeQQ [r|
                def foo:
                    node1 = 5
                    5

                def bar:
                    "bar"

                def main:
                    node1 = 1
                    print bar
                |]
