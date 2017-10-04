{-# LANGUAGE RankNTypes #-}
module NodeEditor.React.IsRef where

import Common.Prelude
import NodeEditor.Event.UI        (UIEvent)
import NodeEditor.React.Model.App (App)
import React.Flux


class (Eq r, Typeable r) => IsRef r where
    dispatch :: r -> UIEvent -> [SomeStoreAction]
    --dt :: r -> Lens' a App

class (Eq a, IsRef (ReactStore a), StoreData a) => HasApp a where
    app :: Lens' a App
