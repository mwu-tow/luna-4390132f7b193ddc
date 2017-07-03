{-# LANGUAGE JavaScriptFFI #-}

module JS.Lexer
    ( lunaClass
    ) where

import           Common.Prelude
import           GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)


foreign import javascript safe "lexerClasses.lunaClass($1)"
    lunaClass' :: JSVal -> JSVal

lunaClass :: String -> Maybe String
lunaClass = pFromJSVal . lunaClass' . pToJSVal
