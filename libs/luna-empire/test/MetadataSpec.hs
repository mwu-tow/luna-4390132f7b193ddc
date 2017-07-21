{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module MetadataSpec (spec) where

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
import qualified Empire.Data.Graph               as Graph (clsClass, code, codeMarkers, breadcrumbHierarchy)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Empire                   (CommunicationEnv (..), Empire)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Parser.Parser  as Parser (ReparsingChange (..), ReparsingStatus (..))
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem(Definition))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Point           (Point (Point))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           LunaStudio.Data.Vector2         (Vector2(..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))

import           Empire.Prelude
import           Luna.Prelude                    (normalizeQQ)

import           Test.Hspec                      (Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                                  shouldNotBe, shouldSatisfy, shouldStartWith, xit, Expectation)

import           EmpireUtils

import           Text.RawString.QQ               (r)

import qualified Luna.IR                         as IR

codeWithMetadata = [r|def foo:
    «10»pi = 3.14

def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «4»lala = 17.0
        «11»buzz = x: y:
            «9»x * y
        «5»pi = 3.14
        «6»n = buzz a lala
        «7»m = buzz b pi
        «8»m + n
    «2»c = 4.0
    «3»bar = foo 8.0 c

### META {"metas":[]}
|]

code = [r|def foo:
    «1»pi = 3.14

def main:
    «0»c = 4.0
|]

oneNode = [r|def main:
    «0»pi = 3.14
    None

### META {"metas":[]}
|]

simpleCodeWithMetadata = [r|def foo:
    «1»pi = 3.14

def main:
    «0»c = 4.0

### META {"metas":[{"marker":0,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":33,"_vector2_x":66}}}},{"marker":1,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-33,"_vector2_x":-66}}}}]}
|]

atXPos = ($ def) . (NodeMeta.position . Position.x .~)

spec :: Spec
spec = around withChannels $ parallel $ do
    describe "handles metadata" $ do
        it "dumps metadata from a graph" $ \env -> do
            metadata <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                Graph.dumpMetadata "TestPath"
            length metadata `shouldBe` 12
        it "gets metadata expr" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                Graph.withUnit loc $ runASTOp $ do
                    cls <- use Graph.clsClass
                    ASTRead.getMetadataRef cls
            meta `shouldSatisfy` isJust
        it "shows no metadata if code doesn't have any" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                Graph.withUnit loc $ runASTOp $ do
                    cls <- use Graph.clsClass
                    ASTRead.getMetadataRef cls
            meta `shouldSatisfy` isNothing
        it "puts updated metadata in code" $ \env -> do
            (prevMeta, meta) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                prevMeta <- Graph.dumpMetadata "TestPath"
                Graph.addMetadataToCode "TestPath"
                Graph.FileMetadata meta <- Graph.readMetadata "TestPath"
                return (prevMeta, meta)
            prevMeta `shouldMatchList` meta
        it "puts metadata in code without metadata" $ \env -> do
            (prevMeta, meta) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                prevMeta <- Graph.dumpMetadata "TestPath"
                Graph.addMetadataToCode "TestPath"
                Graph.FileMetadata meta <- Graph.readMetadata "TestPath"
                return (prevMeta, meta)
            prevMeta `shouldMatchList` meta
        it "loads metadata from a file" $ \env -> do
            (zeroMeta, oneMeta) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc simpleCodeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                zeroMeta <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    ref <- preuse $ Graph.codeMarkers . ix 0
                    mapM AST.readMeta ref
                oneMeta <- Graph.withGraph (loc |>= foo ^. Node.nodeId) $ runASTOp $ do
                    ref <- preuse $ Graph.codeMarkers . ix 1
                    mapM AST.readMeta ref
                return (join zeroMeta, join oneMeta)
            zeroMeta `shouldBe` Just (NodeMeta (Position.fromTuple (66,33)) False Nothing)
            oneMeta  `shouldBe` Just (NodeMeta (Position.fromTuple (-66,-33)) False Nothing)
        xit "removes last node in a file with metadata" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Just pi <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    Graph.getNodeIdForMarker 0
                Graph.removeNodes (loc |>= main ^. Node.nodeId) [pi]
                Graph.getCode loc
            code `shouldBe` normalizeQQ [r|
                def main:
                    None

                |]
        xit "copies nodes with metadata" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                [Just foo, Just c, Just bar] <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    mapM Graph.getNodeIdForMarker [1,2,3]
                Graph.prepareCopy (loc |>= main ^. Node.nodeId) [foo, c, bar]
            code `shouldBe` ""
