{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Field where

import           Common.Prelude
import           NodeEditor.React.IsRef      (IsRef, dispatch)
import           React.Flux
import qualified React.Flux                    as React
import qualified NodeEditor.Event.Keys        as Keys
import           NodeEditor.React.Model.Field


name :: JSString
name = "field"

data Mode = Disabled
          | Single
          | MultiLine
          deriving (Eq)

handleKeys :: IsRef r => Field r -> Event -> KeyboardEvent -> [SomeStoreAction]
handleKeys model e k
  | Keys.withoutMods k Keys.esc   = mayDispatch $ model ^. onCancel
  | Keys.withoutMods k Keys.enter = mayDispatch $ model ^. onAccept
  | otherwise                     = [stop]
  where
      stop = stopPropagation e
      mayDispatch = maybe [stop] (\ev -> stop: dispatch (model ^. ref) (ev $ target e "value"))

handleChange :: IsRef r => Field r -> [PropertyOrHandler [SomeStoreAction]] -> [PropertyOrHandler [SomeStoreAction]]
handleChange model = case model ^. onEdit of
    Just ev -> (onChange (dispatch (model ^. ref) . ev . (`target` "value")) :)
    Nothing -> id


handleBlur :: IsRef r => Field r -> [PropertyOrHandler [SomeStoreAction]] -> [PropertyOrHandler [SomeStoreAction]]
handleBlur model = case model ^. onCancel of
    Just ev -> ((onBlur $ \e _ -> dispatch (model ^. ref) $ ev $ target e "value") :)
    Nothing -> id

field :: IsRef r => [PropertyOrHandler ViewEventHandler] -> ReactView (Mode, Field r)
field ph' = React.defineView name $ \(mode, model) -> let
    editPh = handleChange model
           $ handleBlur model
           $ onKeyDown   (\e k -> handleKeys model e k)
           : onMouseDown (\e _ -> [stopPropagation e])
           : (if isJust (model ^. onEdit) then "value" else "defaultValue") $= convert (model ^. content)
           : ph'
    in case mode of
        Disabled  -> div_ $ elemString $ convert $ model ^. content
        MultiLine -> textarea_ editPh mempty
        Single    -> input_ editPh

field_ :: IsRef r => Mode -> [PropertyOrHandler ViewEventHandler] -> JSString -> Field r -> ReactElementM ViewEventHandler ()
field_ mode ph key model = React.viewWithSKey (field ph) key (mode, model) mempty

multilineField_ :: IsRef r => [PropertyOrHandler ViewEventHandler] -> JSString -> Field r -> ReactElementM ViewEventHandler ()
multilineField_ = field_ MultiLine

singleField_ :: IsRef r => [PropertyOrHandler ViewEventHandler] -> JSString -> Field r -> ReactElementM ViewEventHandler ()
singleField_ = field_ Single
