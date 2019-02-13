{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Lexer
    ( installLexer
    ) where

import           Common.Data.JSON                   (fromJSONVal, toJSONVal)
import           Common.Prelude
import           Data.Aeson.Types                   (FromJSON, ToJSON)
import           Data.Text.Position                 (Delta)
import           Data.Text32                        (Text32)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal                      (FromJSVal, ToJSVal, fromJSVal, toJSVal)
import           GHCJS.Marshal.Pure                 (pFromJSVal, pToJSVal)
import           JavaScript.Array                   (JSArray)
import qualified JavaScript.Array                   as JSArray
import           Luna.Syntax.Text.Analysis.Disabled (ColumnStack, tagDisabled')
import           Luna.Syntax.Text.Lexer             (EntryPoint, EntryStack, StrType, Symbol, Token)
import qualified Luna.Syntax.Text.Lexer             as Lexer

foreign import javascript safe "atomCallbackTextEditor.setLexer($1)"
    setLexer' :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.unsetLexer()"
    unsetLexer :: IO ()

foreign import javascript safe "{length: $1, tags: $2}"
    exportToken' :: Int -> JSArray -> JSVal

foreign import javascript safe "{tokens: $1, stack: $2}"
    exportResult :: JSVal -> JSVal-> JSVal

data LineStack = LineStack EntryStack ColumnStack deriving (Generic, Show)

instance FromJSON  Delta
instance ToJSON    Delta
instance FromJSVal Delta where fromJSVal = fromJSONVal
instance ToJSVal   Delta where toJSVal = toJSONVal
instance FromJSON  StrType
instance ToJSON    StrType
instance FromJSVal StrType where fromJSVal = fromJSONVal
instance ToJSVal   StrType where toJSVal = toJSONVal
instance FromJSON  EntryPoint
instance ToJSON    EntryPoint
instance FromJSVal EntryPoint where fromJSVal = fromJSONVal
instance ToJSVal   EntryPoint where toJSVal = toJSONVal
instance FromJSON  LineStack
instance ToJSON    LineStack
instance FromJSVal LineStack where fromJSVal = fromJSONVal
instance ToJSVal   LineStack where toJSVal = toJSONVal
instance Default   LineStack

exportToken :: (ColumnStack, Token (Symbol, EntryStack)) -> [JSVal]
exportToken (columnStack, token) =
    [ exportToken' (fromIntegral $ token ^. Lexer.span)
                   (JSArray.fromList $ map (pToJSVal . (convert :: Text32 -> String)) $ applyDisabled $ Lexer.getTags $ token ^. Lexer.element . _1)
    , exportToken' (fromIntegral $ token ^. Lexer.offset)
                   (JSArray.fromList [])
    ]
    where
        applyDisabled = if null columnStack then id else ("Disable":)

setLexer :: (Maybe LineStack -> Text32 -> IO ([(ColumnStack, Token (Symbol, EntryStack))], LineStack)) -> IO (IO ())
setLexer lexer = do
    wrappedCallback <- syncCallback2' $ \stackVal inputVal -> timeIt "lexerJSCalback" $ do
        let input = convert (pFromJSVal inputVal :: String)
        stack              <- fromJSVal stackVal
        (tokens, newStack) <- lexer (join stack) input                            <!!>  "run lexer"
        tokensVal          <- toJSValListOf (concatMap exportToken tokens) <!!> "serialize"
        newStackVal        <- toJSVal newStack
        return $ exportResult tokensVal newStackVal
    setLexer' wrappedCallback
    return $ unsetLexer >> releaseCallback wrappedCallback

installLexer :: IO (IO ())
installLexer = setLexer $ \mayStack line -> timeIt "runLexer" $ do
    let LineStack entryStack columnStack = fromMaybe def mayStack
        tokensWEntry   = Lexer.runLexer entryStack line
        tokensWEC      = tagDisabled' columnStack tokensWEntry
        newColumnStack = last tokensWEC ^. _1
        newEntryStack  = last tokensWEC ^. _2 . Lexer.element . _2
        newStack = LineStack newEntryStack newColumnStack
    return (tokensWEC, newStack)
