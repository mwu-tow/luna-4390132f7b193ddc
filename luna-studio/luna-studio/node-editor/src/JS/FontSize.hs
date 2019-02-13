module JS.FontSize where

import           Common.Prelude
import           GHCJS.Marshal.Pure (pFromJSVal)

foreign import javascript safe "fontSize.getFontSize()" getFontSize' :: IO JSVal

getFontSize :: IO Double
getFontSize = pFromJSVal <$> getFontSize'
