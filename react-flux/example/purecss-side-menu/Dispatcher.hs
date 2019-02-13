module Dispatcher (
    -- re-export NavPageId so views don't have to import NavStore directly
    NavPageId(..)
  , changePageTo
) where

import React.Flux
import NavStore

changePageTo :: ReactStore NavState -> NavPageId -> [SomeStoreAction]
changePageTo navStore p = [SomeStoreAction navStore $ ChangePageTo p]
