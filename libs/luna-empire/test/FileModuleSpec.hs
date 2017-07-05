{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FileModuleSpec (spec) where

import           Data.List                      (find)
import qualified Data.Text                      as Text
import           Empire.ASTOp                   (runASTOp)
import qualified Empire.Commands.Graph          as Graph
import qualified Empire.Commands.GraphBuilder   as GraphBuilder
import qualified Empire.Commands.Library        as Library
import           LunaStudio.Data.Breadcrumb     (Breadcrumb (..), BreadcrumbItem (..))
import qualified LunaStudio.Data.Breadcrumb     as Breadcrumb
import qualified LunaStudio.Data.Graph          as Graph
import           LunaStudio.Data.GraphLocation  (GraphLocation (..))
import qualified LunaStudio.Data.Node           as Node

import           Luna.Prelude                   (normalizeQQ)
import           Empire.Prelude

import           Test.Hspec                     (Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                                 shouldNotBe, shouldSatisfy, shouldStartWith, xit)

import           EmpireUtils

import           Text.RawString.QQ              (r)


code = [r|def foo:
    5

def bar:
    "bar"

def main:
    print bar
|]

spec :: Spec
spec = around withChannels $ do
    describe "multi-module files" $ do
        it "shows functions at file top-level" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                Graph.getNodes loc
            length nodes `shouldBe` 3
        it "adds function at top-level" $ \env -> do
            u1 <- mkUUID
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                Graph.addNode loc u1 "quux" def
                Graph.getNodes loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
        it "adds function at top-level as def" $ \env -> do
            u1 <- mkUUID
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                Graph.addNode loc u1 "def quux" def
                Graph.getNodes loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
        it "enters just added function" $ \env -> do
            u1 <- mkUUID
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                n <- Graph.addNode loc u1 "quux" def
                Graph.getNodes (GraphLocation "TestPath" (Breadcrumb [Definition (n ^. Node.nodeId)]))
            length nodes `shouldBe` 0
        it "removes function at top-level" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [bar ^. Node.nodeId]
                Graph.getNodes loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isNothing
        it "adds and removes function" $ \env -> do
            u1 <- mkUUID
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                n <- Graph.addNode loc u1 "def quux" def
                Graph.removeNodes loc [n ^. Node.nodeId]
                Graph.getNodes loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isNothing
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
                Graph.loadCode loc code
                Graph.substituteCode "TestPath" 13 14 "10" (Just 14)
