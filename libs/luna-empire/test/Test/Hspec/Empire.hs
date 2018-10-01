module Test.Hspec.Empire (module Test.Hspec.Empire, module X) where

import Test.Hspec.Empire.Graph     as X
import Test.Hspec.Hspec.WithReason as X

import Empire.Prelude

import qualified Data.Text             as Text
import qualified Empire.Commands.Graph as Graph
import qualified Empire.Data.Graph     as Graph

import Control.Concurrent.MVar       (newEmptyMVar)
import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TChan  (newTChan)
import Control.Exception             (bracket)
import Data.Char                     (isSpace)
import Data.List                     (dropWhileEnd)
import Empire.Commands.Library       (createLibrary)
import Empire.Data.Graph             (CommandState (CommandState),
                                      defaultPMState)
import Empire.Empire                 (CommunicationEnv (CommunicationEnv),
                                      Empire, evalEmpire)
import LunaStudio.Data.GraphLocation (GraphLocation (GraphLocation))
import Test.Hspec                    (Expectation, Spec, SpecWith, around,
                                      describe, parallel, shouldBe)
import Text.RawString.QQ             (r)


withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ pure ()) where
    createChannels = CommunicationEnv
        <$> atomically newTChan <*> newEmptyMVar <*> newEmptyMVar

runTests :: String -> SpecWith CommunicationEnv -> Spec
runTests = around withChannels . parallel .: describe

emptyCodeTemplate :: Text
emptyCodeTemplate = [r|
import Std.Base

def main:
    None
|]

normalizeLunaCode :: Text -> Text
normalizeLunaCode str = Text.intercalate "\n" $ Text.drop minWs <$> allLines where
    trimTrailingSpaces = Text.dropWhileEnd isSpace
    trimEmptyLines     = dropWhileEnd Text.null . dropWhile Text.null
    indentLength       = Text.length . Text.takeWhile isSpace
    allLines = trimEmptyLines $ trimTrailingSpaces <$> Text.lines str
    minWs    = minimum $ indentLength <$> filter (not . Text.null) allLines

codeCheck :: Text -> (Text -> Expectation)
codeCheck expectedCode = \resultCode ->
    Text.strip resultCode `shouldBe` normalizeLunaCode expectedCode

testCase
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCase initialCode expectedCode action env = let
        filePath = "/TestPath"
        topGl    = GraphLocation filePath def
        execute  = do
            createLibrary Nothing filePath
            Graph.loadCode topGl $ normalizeLunaCode initialCode
            let mainNodeName = "main"
                withMain mainNodeId = do
                    let gl = topGl |>= mainNodeId
                    mockNodesLayout gl
                    pure gl
            mainNodeId <- findNodeIdByName topGl mainNodeName
            gl <- maybe (pure topGl) withMain mainNodeId
            action gl
            Graph.getCode gl
    in do
        pm         <- defaultPMState
        resultCode <- evalEmpire env (CommandState pm def) execute
        codeCheck expectedCode resultCode

--[TODO]: This function is copy paste of testCase and is meant to be removed soon, when markers are removed from Luna
testCaseWithMarkers
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCaseWithMarkers initialCode expectedCode action env = let
        filePath = "/TestPath"
        topGl    = GraphLocation filePath def
        execute  = do
            createLibrary Nothing filePath
            Graph.loadCode topGl $ normalizeLunaCode initialCode
            let mainNodeName = "main"
                withMain mainNodeId = do
                    let gl = topGl |>= mainNodeId
                    mockNodesLayout gl
                    pure gl
            mainNodeId <- findNodeIdByName topGl mainNodeName
            gl <- maybe (pure topGl) withMain mainNodeId
            action gl
            Graph.withGraph gl (use Graph.code)
    in do
        pm         <- defaultPMState
        resultCode <- evalEmpire env (CommandState pm def) execute
        codeCheck expectedCode resultCode
