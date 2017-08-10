{-# LANGUAGE JavaScriptFFI #-}

module JS.Lexer
    ( installLexer
    ) where

import           Common.Data.JSON       (fromJSONVal)
import           Common.Prelude
import           Data.Aeson.Types       (FromJSON, ToJSON)
import           Data.Text32            (Text32)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal          (FromJSVal, ToJSVal, fromJSVal, toJSVal)
import           GHCJS.Marshal.Pure     (pFromJSVal, pToJSVal)
import           JavaScript.Array       (JSArray)
import qualified JavaScript.Array       as JSArray
import           Luna.Syntax.Text.Lexer (EntryPoint, EntryStack, StrType, Symbol, Token)
import qualified Luna.Syntax.Text.Lexer as Lexer


foreign import javascript safe "atomCallbackTextEditor.setLexer($1)"
    setLexer' :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.unsetLexer()"
    unsetLexer :: IO ()

foreign import javascript safe "{length: $1, tags: $2}"
    exportToken' :: Int -> JSArray -> JSVal

foreign import javascript safe "{tokens: $1, stack: $2}"
    exportResult :: JSVal -> JSVal-> JSVal

instance FromJSON  StrType
instance ToJSON    StrType
instance FromJSVal StrType where fromJSVal = fromJSONVal
instance ToJSVal   StrType where toJSVal = toJSVal
instance FromJSON  EntryPoint
instance ToJSON    EntryPoint
instance FromJSVal EntryPoint where fromJSVal = fromJSONVal
instance ToJSVal   EntryPoint where toJSVal = toJSVal

exportToken :: Token (Symbol, EntryStack) -> [JSVal]
exportToken token =
    [ exportToken' (fromIntegral $ token ^. Lexer.span)
                   (JSArray.fromList $ map (pToJSVal . (convert :: Text32 -> String)) $ Lexer.getTags $ token ^. Lexer.element . _1)
    , exportToken' (fromIntegral $ token ^. Lexer.offset)
                   (JSArray.fromList [])
    ]

setLexer :: (Maybe EntryStack -> Text32 -> IO [Token (Symbol, EntryStack)]) -> IO (IO ())
setLexer lexer = do
    wrappedCallback <- syncCallback2' $ \stackVal inputVal -> timeIt "lexerJSCalback" $ do
        let input = convert (pFromJSVal inputVal :: String)
        stack       <- fromJSVal stackVal
        tokens      <- lexer stack input                            <!!>  "run lexer"
        tokensVal   <- toJSValListOf (concatMap exportToken tokens) <!!> "serialize"
        newStackVal <- toJSVal $ last tokens ^. Lexer.element . _2
        return $ exportResult tokensVal newStackVal
    setLexer' wrappedCallback
    return $ unsetLexer >> releaseCallback wrappedCallback

installLexer :: IO (IO ())
installLexer = setLexer $ \mayStack -> timeIt "runLexer" . return . Lexer.runLexer (fromMaybe def mayStack)
