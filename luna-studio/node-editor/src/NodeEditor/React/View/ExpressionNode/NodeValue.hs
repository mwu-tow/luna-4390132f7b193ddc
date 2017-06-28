{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.NodeValue where

import           Common.Prelude
import qualified Data.Text                                  as Text
import qualified LunaStudio.Data.Error                      as LunaError
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, Value (..))
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import qualified NodeEditor.React.View.Style                as Style
import           React.Flux                                 hiding (image_)
import qualified React.Flux                                 as React


nodeValueName :: JSString
nodeValueName = "node-value"

nodeValue_ :: ExpressionNode -> ReactElementM ViewEventHandler ()
nodeValue_ n = React.view nodeValue n mempty

nodeValue :: ReactView (ExpressionNode)
nodeValue = React.defineView nodeValueName $ \n ->
    -- TODO[JK]: Squeeze these divs into one
    div_
        [ "key"       $= "results"
        , "className" $= Style.prefixFromList ["node__results", "node-translate"]
        ] $ do
        div_
            [ "key"       $= "shortValue"
            , "className" $= Style.prefixFromList ["node__short-value", "noselect"]
            , onDoubleClick $ \e _ -> [stopPropagation e]
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
