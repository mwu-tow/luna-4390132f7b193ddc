module Main (main) where

import React.Flux
import App
import NavStore (initHistory, createNavPageStore)

main :: IO ()
main = do
    navStore <- createNavPageStore
    initHistory navStore
    reactRender "side-menu-app" (myApp navStore) ()
