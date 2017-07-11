{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.NodeValue where

import           Common.Prelude
import qualified Data.Text                                  as Text
import qualified LunaStudio.Data.Error                      as LunaError
import qualified NodeEditor.Event.UI                        as UI
import qualified NodeEditor.React.Event.Node                as Node
import           NodeEditor.React.Model.App                 (App)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, Value (..))
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.React.Store                     (Ref, dispatch)
import qualified NodeEditor.React.View.Style                as Style
import           React.Flux                                 hiding (image_)
import qualified React.Flux                                 as React


nodeValueName :: JSString
nodeValueName = "node-value"

nodeValue_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeValue_ ref n = React.view nodeValue (ref, n) mempty

nodeValue :: ReactView (Ref App, ExpressionNode)
nodeValue = React.defineView nodeValueName $ \(ref, n) ->
    -- TODO[JK]: Squeeze these divs into one
    div_
        [ "key"       $= "results"
        , "className" $= Style.prefixFromList ["node__results", "node-translate"]
        ] $ do
        div_
            [ "key"       $= "shortValue"
            , "className" $= Style.prefixFromList ["node__short-value", "noselect"]
            , onDoubleClick $ \e _ -> [stopPropagation e]
            , onClick       $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.ShowFullError $ n ^. Node.nodeLoc
            ] $ elemString $ strValue n

strValue :: ExpressionNode -> String
strValue n = case n ^. Node.value of
    Nothing -> ""
    Just (ShortValue value) -> Text.unpack value
    Just (Error      msg  ) -> showError msg --limitString errorLen (convert $ showError msg)

showError :: LunaError.Error -> String
showError = showErrorSep ""

showErrorSep :: String -> LunaError.Error -> String
showErrorSep sep err = case err of
    LunaError.Error LunaError.CompileError msg -> "Compile error: " <> sep <> convert msg
    LunaError.Error LunaError.RuntimeError msg -> "Runtime error: " <> sep <> convert msg
