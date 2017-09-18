{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.PortControl
    ( portLabel_
    , portControl_
    ) where

import           Common.Prelude              hiding (group)
import qualified JS.Config                   as Config
import           LunaStudio.Data.Port        (InPortIndex (Arg))
import qualified LunaStudio.Data.Port        as PortAPI
import qualified LunaStudio.Data.PortDefault as PortDefault
import           LunaStudio.Data.PortRef     (InPortRef (InPortRef))
import           LunaStudio.Data.TypeRep     (TypeRep (TCons))
import qualified NodeEditor.Event.UI         as UI
import qualified NodeEditor.React.Event.Node as Node
import           NodeEditor.React.Model.App  (App)
import           NodeEditor.React.Model.Node (NodeLoc)
import           NodeEditor.React.Model.Port (InPort)
import qualified NodeEditor.React.Model.Port as Port
import           NodeEditor.React.Store      (Ref, dispatch)
import qualified NodeEditor.React.View.Style as Style
import           NodeEditor.State.Action     (InitValue (Continous, Discrete))
import           React.Flux                  as React


roundTo :: Int -> Double -> Double
roundTo limit a =  round' (f * a) / f
  where
    round' v = fromIntegral (round v :: Integer)
    f        = fromIntegral (10^limit :: Integer)

integer :: String -> String
integer a = if null a then "" else case last a of
    '.' -> init a
    _   -> integer $ init a

fractional :: String -> String
fractional a = case a of
    []     -> ""
    '.':xs -> take 3 xs
    _:xs   -> fractional xs

fractionalExp :: String -> String
fractionalExp a = case a of
        []     -> ""
        '.':xs -> take 3 xs <> "…" <> ePart xs
        _:xs   -> fractionalExp xs
    where
        ePart b =
            case b of
                []     -> ""
                'e':bs -> 'e':bs
                _:bs   -> ePart bs

isExpNotation :: String -> Bool
isExpNotation a =
    case a of
        []    -> False
        'e':_ -> True
        _:xs  -> isExpNotation xs

portControlId, labelPrefix, controlPrefix :: JSString
portControlId = Config.prefix "focus-portcontrol"
labelPrefix   = Config.prefix "label-"
controlPrefix = Config.prefix "control-"

portLabel_ :: InPort -> ReactElementM ViewEventHandler ()
portLabel_ port = case port ^. Port.portId of
    [Arg _] -> div_
                   [ "key"       $= (labelPrefix <> jsShow (port ^. Port.portId))
                   , "className" $= Style.prefix "node__label"
                   ] $ elemString . convert $ port ^. Port.name
    _       -> return ()

portControl_ :: Ref App -> NodeLoc -> InPort -> ReactElementM ViewEventHandler ()
portControl_ ref' nl' port' = React.viewWithSKey portControl (jsShow $ port' ^. Port.portId) (ref', nl', port') mempty
    where
        mkPortControl ref nl port = do
            let valueClass  = case port ^. Port.state of
                    PortAPI.NotConnected  -> "node__ctrl--not-connected"
                    PortAPI.Connected     -> "node__ctrl--connected"
                    PortAPI.WithDefault _ -> "node__ctrl--with-default"
                portRef     = InPortRef nl $ port ^. Port.portId
            div_
                [ "key"       $= (controlPrefix <> jsShow (port ^. Port.portId))
                , "className" $= Style.prefixFromList [ "node__ctrl", valueClass ]
                ] $ case port ^. Port.state of
                    PortAPI.NotConnected -> do
                        let tConsElem :: PortDefault.PortValue -> ReactElementM ViewEventHandler ()
                            tConsElem zeroValue = button_
                                [ "className" $= Style.prefix "ctrl--set"
                                , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef $ PortDefault.Constant zeroValue
                                ] $ elemString "set"
                        case port ^. Port.valueType of
                            TCons "Int"  _ -> tConsElem $ PortDefault.IntValue  def
                            TCons "Real" _ -> tConsElem $ PortDefault.RealValue def
                            TCons "Text" _ -> tConsElem $ PortDefault.TextValue def
                            TCons "Bool" _ -> tConsElem $ PortDefault.BoolValue False
                            _              -> elemString ""
                    PortAPI.Connected -> elemString "" -- TODO get current value from the connected port
                    PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType of
                        TCons "Int" _ -> do
                            let value       = fromMaybe 0 $ defVal ^? PortDefault._Constant . PortDefault._IntValue
                            div_
                                [ "className" $= Style.prefix "ctrl--slider"
                                --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Discrete value)
                                ] $ elemString $ show value
                        TCons "Real" _ -> do
                            let value = fromMaybe 0.0 $ defVal ^? PortDefault._Constant . PortDefault._RealValue
                            div_
                                [ "className" $= Style.prefixFromList [ "ctrl--slider", "ctrl--slider--double" ]
                                --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Continous value)
                                ] $ do
                                    let val = show $ roundTo 3 value
                                    div_ [ "className" $= Style.prefix "ctrl--slider__left"  ] $ elemString $ integer val
                                    div_ [ "className" $= Style.prefix "ctrl--slider__right" ] $ elemString $ (if isExpNotation val then fractionalExp else fractional) val
                        TCons "Text" _ -> do
                            let value = fromMaybe "" $ defVal ^? PortDefault._Constant . PortDefault._TextValue
                                defaultValue val = PortDefault.Constant $ PortDefault.TextValue val
                            input_
                                [ "id" $= portControlId
                                , "value" $= convert value
                                , onMouseDown $ \e _ -> [stopPropagation e]
                                , onKeyDown   $ \e k -> let val = target e "value" in stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortApplyString k portRef $ defaultValue val)
                                , onChange    $ \e   -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.PortEditString portRef $ defaultValue val
                                ]
                        TCons "Bool" _ -> do
                            let value = fromMaybe True $ defVal ^? PortDefault._Constant . PortDefault._BoolValue
                                defaultValue = PortDefault.Constant $ PortDefault.BoolValue $ not value
                            div_
                                [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef defaultValue
                                , "className" $= Style.prefix ("ctrl--bool--" <> jsShow value)
                                ] mempty
                        _ -> elemString ""

        portControl = React.defineView "portControl" $ \(ref, nl, port) -> case port ^. Port.portId of
            [Arg {}] -> mkPortControl ref nl port
            []       -> mkPortControl ref nl port
            _        -> div_
                [ "key"       $= "self-ctrl"
                , "className" $= Style.prefixFromList ["node__ctrl", "node__ctrl--self"]
                ] $ elemString ""
