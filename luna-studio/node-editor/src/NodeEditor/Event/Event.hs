{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Event where

import           Common.Analytics            (IsTrackedEvent (..), (<.$>))
import           Common.Prelude
import qualified NodeEditor.Event.Atom       as Atom
import qualified NodeEditor.Event.Batch      as Batch
import qualified NodeEditor.Event.Connection as Connection
import           NodeEditor.Event.Shortcut   (ShortcutEvent)
import           NodeEditor.Event.UI         (UIEvent)


data Event = Init
           | Batch            Batch.Event
           | Connection  Connection.Event
           | Atom              Atom.Event
           | Shortcut       ShortcutEvent
           | UI                   UIEvent
           deriving (Generic, Show, NFData)

makeLenses ''Event

name :: Getter Event String
name = to $ head . words . show

instance IsTrackedEvent Event where
    eventName event = ("NodeEditor.Event." <> (event ^. name)) <.$> case event of
        Init         -> Just ""
        Batch b      -> eventName b
        Connection c -> eventName c
        Atom a       -> eventName a
        Shortcut s   -> eventName s
        UI u         -> eventName u
