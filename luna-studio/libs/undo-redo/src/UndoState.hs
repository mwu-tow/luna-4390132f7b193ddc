{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UndoState where


import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Binary          (Binary)
import qualified Data.Set             as Set
import           Data.UUID.Types      (UUID)
import           Prologue             hiding (throwM)
import           Util                 as Util
import qualified ZMQ.Bus.Trans        as Bus

import qualified LunaStudio.API.Topic     as Topic


type GuiID   = UUID
type ReqUUID = UUID
data UndoMessage where
    UndoMessage :: (Binary undoReq, Binary redoReq) => GuiID -> ReqUUID -> Topic.Topic -> undoReq -> Topic.Topic -> redoReq -> UndoMessage

instance Eq UndoMessage where
    (UndoMessage _ reqID1 _ _ _ _) == (UndoMessage _ reqID2 _ _ _ _) = (reqID1 == reqID2)
instance Show UndoMessage where
    show (UndoMessage guiID reqID topic1 _ topic2 _) =
        "UndoMessage " <> show guiID <> " " <> show reqID <> " " <> show topic1 <> " " <> show topic2


data UndoState = UndoState { _undo    :: [UndoMessage]
                           , _redo    :: [UndoMessage]
                           , _history :: [UndoMessage]
                           }
makeLenses ''UndoState

newtype UndoT b a = Undo {runUndo :: StateT UndoState b a}
    deriving (Applicative, Functor, Monad, MonadState UndoState, MonadIO, MonadThrow, MonadTrans, MonadCatch)

type Undo = UndoT Bus.BusT
type UndoPure = UndoT IO

data Action where
    Action :: (Binary req) => Topic.Topic -> req -> Action

data Act = ActUndo | ActRedo
