
{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( activeLocation
    , insertCode
    , pushInterpreterUpdate
    , pushStatus
    , setBuffer
    , setClipboard
    , subscribeDiffs
    , subscribeEventListenerInternal
    ) where

import           Common.Data.JSON              (fromJSONVal)
import           Common.Prelude                hiding (toList)
import           Control.Monad.Trans.Maybe     (MaybeT (MaybeT), runMaybeT)
import qualified Data.Text                     as Text
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure            (PFromJSVal (pFromJSVal), PToJSVal (pToJSVal))
import           LunaStudio.Data.Diff          (Diff (Diff))
import qualified LunaStudio.Data.Diff          as Diff
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Point         (Point (Point))
import qualified LunaStudio.Data.Point         as Point
import           TextEditor.Event.Internal     (InternalEvent, InternalEvent (..))
import           TextEditor.Event.Text         (TextEvent (TextEvent))
import qualified TextEditor.Event.Text         as TextEvent


foreign import javascript safe "atomCallbackTextEditor.insertCode($1, $2, $3, $4)"
    insertCode' :: JSString -> JSVal -> JSVal -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.setBuffer($1, $2)"
    setBuffer :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.setClipboard($1, $2)"
    setClipboard :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushStatus($1, $2, $3)"
    pushStatus :: JSString -> JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushInterpreterUpdate($1, $2)"
    pushInterpreterUpdate' :: JSString -> JSVal -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeEventListenerInternal($1)"
    subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeEventListenerInternal()"
    unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeDiffs($1)"
    subscribeDiffs' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeDiffs()"
    unsubscribeDiffs' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.uri"    getPath   :: JSVal -> JSVal
foreign import javascript safe "$1.start"  getStart  :: JSVal -> JSVal
foreign import javascript safe "$1.end"    getEnd    :: JSVal -> JSVal
foreign import javascript safe "$1.text"   getText   :: JSVal -> JSVal
foreign import javascript safe "$1.cursor" getCursor :: JSVal -> JSVal
foreign import javascript safe "$1.column" getColumn :: JSVal -> Int
foreign import javascript safe "$1.row"    getRow    :: JSVal -> Int
foreign import javascript safe "{column: $1, row: $2}" mkPoint   :: Int -> Int -> JSVal
foreign import javascript safe "globalRegistry.activeLocation" activeLocation' :: IO JSVal

instance PFromJSVal Point where
    pFromJSVal jsval = Point (getColumn jsval) (getRow jsval)

instance PToJSVal Point where
    pToJSVal point = mkPoint (point ^. Point.column) (point ^. Point.row)

instance FromJSVal Diff where
    fromJSVal jsval = return $ Just $ Diff
        (pFromJSVal $ getStart  jsval)
        (pFromJSVal $ getEnd    jsval)
        (pFromJSVal $ getText   jsval)
        (pFromJSVal $ getCursor jsval)

instance FromJSVal GraphLocation where fromJSVal = fromJSONVal
instance FromJSVal InternalEvent where fromJSVal = fromJSONVal

instance FromJSVal TextEvent where
    fromJSVal jsval = runMaybeT $ do
        location <- MaybeT activeLocation
        diffs    <- MaybeT $ fromJSVal jsval
        return $ TextEvent location diffs

activeLocation :: MonadIO m => m (Maybe GraphLocation)
activeLocation = liftIO $ fromJSVal =<< activeLocation'

subscribeDiffs :: (TextEvent -> IO ()) -> IO (IO ())
subscribeDiffs callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ \js -> withJustM_ (fromJSVal js) callback
    subscribeDiffs' wrappedCallback
    return $ unsubscribeDiffs' wrappedCallback >> releaseCallback wrappedCallback

insertCode :: TextEvent -> IO ()
insertCode textEvent = do
    let uri   = textEvent ^. TextEvent.filePath . to convert
        diffs = textEvent ^. TextEvent.diffs
    forM_ diffs $ \diff -> do
        let start = diff ^. Diff.start . to pToJSVal
            end   = diff ^. Diff.end   . to pToJSVal
            text  = diff ^. Diff.newText . to Text.unpack . to convert
        insertCode' uri start end text

subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
subscribeEventListenerInternal callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ \jsval -> withJustM (fromJSVal jsval) callback
    subscribeEventListenerInternal' wrappedCallback
    return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback

pushInterpreterUpdate :: MonadIO m => String -> Maybe Text -> m ()
pushInterpreterUpdate name value = liftIO $ pushInterpreterUpdate' (convert name) (pToJSVal value)
