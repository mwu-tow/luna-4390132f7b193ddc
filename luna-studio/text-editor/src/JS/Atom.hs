
{-# LANGUAGE JavaScriptFFI #-}

module JS.Atom
    ( pushCode
    , pushBuffer
    , pushStatus
    , subscribeDiff
    , subscribeEventListenerInternal
    ) where


import           Common.Prelude
import qualified Data.Text                 as Text
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure        (PFromJSVal (pFromJSVal), PToJSVal (pToJSVal))
import           LunaStudio.Data.Point     (Point (Point))
import qualified LunaStudio.Data.Point     as Point
import           TextEditor.Event.Internal (InternalEvent, InternalEvent (..))
import           TextEditor.Event.Text     (TextEvent (TextEvent))
import qualified TextEditor.Event.Text     as TextEvent


foreign import javascript safe "atomCallbackTextEditor.pushCode($1, $2, $3, $4)"
    pushCode' :: JSString -> JSVal -> JSVal -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushBuffer($1, $2)"
    pushBuffer :: JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.pushStatus($1, $2, $3)"
    pushStatus :: JSString -> JSString -> JSString -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeEventListenerInternal($1)"
    subscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeEventListenerInternal()"
    unsubscribeEventListenerInternal' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "atomCallbackTextEditor.subscribeDiff($1)"
    subscribeDiff' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "($1).unsubscribeDiff()"
    unsubscribeDiff' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$1.event"  getEvent  :: JSVal -> JSVal
foreign import javascript safe "$1.uri"    getPath   :: JSVal -> JSVal
foreign import javascript safe "$1.start"  getStart  :: JSVal -> JSVal
foreign import javascript safe "$1.end"    getEnd    :: JSVal -> JSVal
foreign import javascript safe "$1.text"   getText   :: JSVal -> JSVal
foreign import javascript safe "$1.cursor" getCursor :: JSVal -> JSVal
foreign import javascript safe "$1.column" getColumn :: JSVal -> Int
foreign import javascript safe "$1.row"    getRow    :: JSVal -> Int
foreign import javascript safe "{column: $1, row: $2}" mkPoint   :: Int -> Int -> JSVal

foreign import javascript safe "function() {if($1.hasOwnProperty(selections)) { return $1.selections };}()"
    getSelections :: JSVal -> JSVal

instance PFromJSVal Point where
    pFromJSVal jsval = Point (getColumn jsval) (getRow jsval)

instance PToJSVal Point where
    pToJSVal point = mkPoint (point ^. Point.column) (point ^. Point.row)

jsvalToText :: JSVal -> TextEvent
jsvalToText jsval = result where
    filepath = pFromJSVal $ getPath jsval
    start    = pFromJSVal $ getStart jsval
    end      = pFromJSVal $ getEnd jsval
    text     = pFromJSVal $ getText jsval
    cursor   = pFromJSVal $ getCursor jsval
    result   = TextEvent filepath start end text $ Just cursor

jsvalToInternalEvent :: JSVal -> InternalEvent
jsvalToInternalEvent jsval = result where
    event = read $ pFromJSVal $ getEvent jsval
    filepath = pFromJSVal $ getPath jsval
    -- maybeSelections = GHCJSInternal.fromJSVal $ getSelections jsval
    result = InternalEvent event filepath Nothing

subscribeDiff :: (TextEvent -> IO ()) -> IO (IO ())
subscribeDiff callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . jsvalToText
    subscribeDiff' wrappedCallback
    return $ unsubscribeDiff' wrappedCallback >> releaseCallback wrappedCallback

pushCode :: TextEvent -> IO ()
pushCode = do
    uri   <- view TextEvent.filePath
    start <- pToJSVal . view TextEvent.start
    end   <- pToJSVal . view TextEvent.end
    text  <- view TextEvent.text
    return $ pushCode' (convert uri) start end $ convert $ Text.unpack text

subscribeEventListenerInternal :: (InternalEvent -> IO ()) -> IO (IO ())
subscribeEventListenerInternal callback = do
    wrappedCallback <- syncCallback1 ContinueAsync $ callback . jsvalToInternalEvent
    subscribeEventListenerInternal' wrappedCallback
    return $ unsubscribeEventListenerInternal' wrappedCallback >> releaseCallback wrappedCallback
