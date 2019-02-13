{-# LANGUAGE OverloadedStrings #-}

module JS.Analytics where

import           Common.Prelude


foreign import javascript safe "analytics.track($1, $2)" track :: JSString -> JSVal -> IO ()
