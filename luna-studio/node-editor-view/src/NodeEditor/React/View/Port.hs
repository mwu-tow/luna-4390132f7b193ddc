{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Port where

import           Common.Prelude
import qualified Data.Aeson                         as Aeson
import           LunaStudio.Data.Constants          (nodePropertiesWidth)
import qualified NodeEditor.Event.Mouse             as Mouse
import qualified NodeEditor.Event.UI                as UI
import           LunaStudio.Data.PortRef            (AnyPortRef, toAnyPortRef)
import qualified NodeEditor.React.Event.Port        as Port
import           NodeEditor.React.Model.Constants   (lineHeight, nodeRadius, nodeRadius', portRadius, portAliasRadius, connectionWidth)
import           NodeEditor.React.Model.Node        (NodeLoc)
import           NodeEditor.React.Model.Port        (AnyPort, AnyPortId (InPortId', OutPortId'), InPortIndex (Arg, Self),
                                                    IsOnly, Mode (..), getPortNumber, isInPort, isInvisible, isSelf,
                                                    portAngleStart, portAngleStop, argumentConstructorOffsetY)
import qualified NodeEditor.React.Model.Port        as Port
import           NodeEditor.React.IsRef             (IsRef, dispatch)
import qualified NodeEditor.React.View.Style        as Style
import           Numeric                            (showFFloat)
import           React.Flux                         hiding (view)
import qualified React.Flux                         as React


name :: JSString
name = "port"

typeOffsetX :: Double
typeOffsetX = 36

typeOffsetY1, typeOffsetY2, typeOffsetY3, typeOffsetY :: Double
typeOffsetY1 = (-3.5)
typeOffsetY2 = 4.5
typeOffsetY3 = 12.5
typeOffsetY  = 20.5

selectAreaWidth :: Double
selectAreaWidth = 8

modeClass :: Mode -> [String]
modeClass Invisible      = ["port--invisible"]
modeClass Inactive       = ["port--inactive"]
modeClass TypeNotMatched = ["port--type-not-matched"]
modeClass Highlighted    = ["port--highlighted"]
modeClass _              = []

jsShow2 :: Double -> JSString
jsShow2 a = convert $ showFFloat (Just 2) a "" -- limit Double to two decimal numbers

handleMouseDown :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.PortEvent $ Port.MouseDown m portRef)
    else []

handleClick :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleClick ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.PortEvent $ Port.Click m portRef)
    else []

handleMouseUp :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseUp ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseUp portRef)

handleMouseEnter :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseEnter ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseEnter portRef)

handleMouseLeave :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseLeave ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseLeave portRef)

port :: IsRef r => ReactView (r, NodeLoc, Int, IsOnly, AnyPort)
port = React.defineView name $ \(ref, nl, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId' []       -> portAlias_ ref nl p
        InPortId' (Self:_) -> portSelf_  ref nl p
        OutPortId' []      -> if isOnly then portSingle_ ref nl p
                                        else portIO_ ref nl p numOfPorts
        _                  -> portIO_ ref nl p numOfPorts

portExpanded :: IsRef r => ReactView (r, NodeLoc, AnyPort)
portExpanded = React.defineView name $ \(ref, nl, p) ->
    case p ^. Port.portId of
        InPortId' (Self:_) -> portSelf_       ref nl p
        _                  -> portIOExpanded_ ref nl p

port_ :: IsRef r => r -> NodeLoc -> AnyPort -> Int -> IsOnly -> ReactElementM ViewEventHandler ()
port_ ref nl p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, nl, numOfPorts, isOnly, p) mempty

portExpanded_ :: IsRef r => r -> NodeLoc -> AnyPort -> ReactElementM ViewEventHandler ()
portExpanded_ ref nl p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, nl, p) mempty

handlers :: IsRef r => r -> AnyPortRef -> [PropertyOrHandler [SomeStoreAction]]
handlers ref portRef = [ onMouseDown  $ handleMouseDown  ref portRef
                       , onMouseUp    $ handleMouseUp    ref portRef
                       , onClick      $ handleClick      ref portRef
                       , onMouseEnter $ handleMouseEnter ref portRef
                       , onMouseLeave $ handleMouseLeave ref portRef
                       ]

portAlias_ :: IsRef r => r -> NodeLoc -> AnyPort -> ReactElementM ViewEventHandler ()
portAlias_ ref nl p = React.viewWithSKey portAlias "port-alias" (ref, nl, p) mempty

portSelf_ :: IsRef r => r -> NodeLoc -> AnyPort -> ReactElementM ViewEventHandler ()
portSelf_ ref nl p = React.viewWithSKey portSelf "port-self" (ref, nl, p) mempty

portSingle_ :: IsRef r => r -> NodeLoc -> AnyPort -> ReactElementM ViewEventHandler ()
portSingle_ ref nl p = React.viewWithSKey portSingle "port-single" (ref, nl, p) mempty

portIO_ :: IsRef r => r -> NodeLoc -> AnyPort -> Int -> ReactElementM ViewEventHandler ()
portIO_ ref nl p numOfPorts = React.viewWithSKey portIO "port-io" (ref, nl, p, numOfPorts) mempty

portAlias :: IsRef r => ReactView (r, NodeLoc, AnyPort)
portAlias = React.defineView "port-alias" $ \(ref, nl, p) -> do
    let portId       = p ^. Port.portId
        portRef      = toAnyPortRef nl portId
        color        = convert $ p ^. Port.color
        className    = Style.prefixFromList $ ["port", "port--alias"] <> modeClass (p ^. Port.mode)
        portHandlers = if isInvisible p then [] else handlers ref portRef
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "r"         $= jsShow2 portAliasRadius
            , "stroke"    $= color
            ] mempty
        circle_
            ( portHandlers <>
            [ "className"      $= Style.prefix "port__select"
            , "key"            $= (jsShow portId <> "b")
            , "r"              $= jsShow2 portAliasRadius
            , "strokeWidth"    $= jsShow2 (portRadius - portAliasRadius - connectionWidth)
            , "strokeLocation" $= "outside"
            ]) mempty

portSelf :: IsRef r => ReactView (r, NodeLoc, AnyPort)
portSelf = React.defineView "port-self" $ \(ref, nl, p) -> do
    let portId       = p ^. Port.portId
        portRef      = toAnyPortRef nl portId
        color        = convert $ p ^. Port.color
        className    = Style.prefixFromList $ ["port", "port--self"] <> modeClass (p ^. Port.mode)
        portHandlers = if isInvisible p then [] else handlers ref portRef
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            ] mempty
        circle_
            ( portHandlers <>
            [ "className" $= Style.prefix "port__select"
            , "key"       $= (jsShow portId <> "b")
            , "r"         $= jsShow2 (portAliasRadius - connectionWidth/2)
            ]) mempty

portSingle :: IsRef r => ReactView (r, NodeLoc, AnyPort)
portSingle = React.defineView "port-single" $ \(ref, nl, p) -> do
    let portId   = p ^. Port.portId
        portRef  = toAnyPortRef nl portId
        portType = toString $ p ^. Port.valueType
        isInput  = isInPort portId
        color    = convert $ p ^. Port.color
        classes  = Style.prefixFromList $ [ "port", "port--o", "port--o--single" ] <> modeClass (p ^. Port.mode)
        r1 :: Double -> JSString
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath :: Double -> Integer -> Integer -> JSString
        svgPath a b c = "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> jsShow b <> " 0 "  <> r1 a <>
                       " L0 "  <> r2   <> " A " <> r2   <> " " <> r2   <> " 1 0 " <> jsShow c <> " 0 -" <> r2   <> " Z "
    g_ [ "className" $= classes ] $ do
        text_
            [ "className" $= Style.prefixFromList ([ "port__type", "noselect" ] <> modeClass (p ^. Port.mode))
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= jsShow2 (-typeOffsetY1)
            , "x"         $= jsShow2 (if isInput then (-typeOffsetX) else typeOffsetX)
            ] $ elemString portType
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 20 0 1 <> svgPath 20 1 0)
            ] mempty
        path_
            ( handlers ref portRef <>
            [ "className" $= Style.prefix "port__select"
            , "key"       $= (jsShow portId <> "b")
            , "d"         $= (svgPath 20 0 1 <> svgPath 20 1 0)
            ]) mempty

portIO :: IsRef r => ReactView (r, NodeLoc, AnyPort, Int)
portIO = React.defineView "port-io" $ \(ref, nl, p, numOfPorts) -> do
    let portId   = p ^. Port.portId
        portRef  = toAnyPortRef nl portId
        portType = toString $ p ^. Port.valueType
        isInput  = isInPort portId
        num      = getPortNumber portId
        color    = convert $ p ^. Port.color
        classes  = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1) ] <> modeClass (p ^. Port.mode)
                              else [ "port", "port--o", "port--o--" <> show (num + 1) ] <> modeClass (p ^. Port.mode)
        svgFlag1 = if isInput then "1"  else "0"
        svgFlag2 = if isInput then "0"  else "1"
        mode     = if isInput then -1.0 else 1.0
        adjust
            | numOfPorts == 1 = typeOffsetY1
            | numOfPorts == 2 = typeOffsetY2
            | numOfPorts == 3 = typeOffsetY3
            | otherwise       = typeOffsetY
        startPortArcX isShape r = r * sin(portAngleStart isShape num numOfPorts r * mode)
        startPortArcY isShape r = r * cos(portAngleStart isShape num numOfPorts r * mode)
        stopPortArcX  isShape r = r * sin(portAngleStop  isShape num numOfPorts r * mode)
        stopPortArcY  isShape r = r * cos(portAngleStop  isShape num numOfPorts r * mode)
        ax isShape = jsShow2 . startPortArcX isShape . (+) nodeRadius
        ay isShape = jsShow2 . startPortArcY isShape . (+) nodeRadius
        bx isShape = jsShow2 . stopPortArcX  isShape . (+) nodeRadius
        by isShape = jsShow2 . stopPortArcY  isShape . (+) nodeRadius
        cx isShape = jsShow2 $ stopPortArcX  isShape nodeRadius'
        cy isShape = jsShow2 $ stopPortArcY  isShape nodeRadius'
        dx isShape = jsShow2 $ startPortArcX isShape nodeRadius'
        dy isShape = jsShow2 $ startPortArcY isShape nodeRadius'
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath a b = "M"  <> ax a b <> " " <> ay a b <>
                     " A " <> r1 b   <> " " <> r1 b   <> " 0 0 " <> svgFlag1 <> " " <> bx a b <> " " <> by a b <>
                     " L " <> cx a   <> " " <> cy a   <>
                     " A " <> r2     <> " " <> r2     <> " 0 0 " <> svgFlag2 <> " " <> dx a   <> " " <> dy a   <>
                     " Z"
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList ([ "port__type", "noselect" ] <> modeClass (p ^. Port.mode))
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= jsShow2 ((lineHeight * fromIntegral num) - adjust)
            , "x"         $= jsShow2 (if isInput then (-typeOffsetX) else typeOffsetX)
            ] $ elemString portType
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "-shape")
            , "fill"      $= color
            , "d"         $= svgPath True 20
            ] mempty
        path_
            ( handlers ref portRef <>
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "-select")
              , "d"         $= svgPath False lineHeight
              ]
            ) mempty

portIOExpanded_ :: IsRef r => r -> NodeLoc -> AnyPort -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref nl p = if isSelf $ p ^. Port.portId then portSelf_ ref nl p else do
    let portId   = p ^. Port.portId
        portRef  = toAnyPortRef nl portId
        portType = toString $ p ^. Port.valueType
        isInput  = isInPort portId
        num      = getPortNumber portId
        n        = if isInput then 1 else 0
        color    = convert $ p ^. Port.color
        px       = jsShow2 (if isInput then (-nodePropertiesWidth/2) else nodePropertiesWidth/2)
        py       = jsShow2 (lineHeight * fromIntegral num)
        classes  = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1)] <> modeClass (p ^. Port.mode)
                              else [ "port", "port--o", "port--o--" <> show (num + 1)] <> modeClass (p ^. Port.mode)
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList ([ "port__type", "noselect" ] <> modeClass (p ^. Port.mode))
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= py
            , "dy"        $= "4px"
            , "dx"        $= jsShow2 (if isInput then (-(16 + nodePropertiesWidth/2)) else 16 + nodePropertiesWidth/2 )
            ] $ elemString portType
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "-shape")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cx"        $= (px <> "px")
            , "cy"        $= (py <> "px")
            ] mempty
        circle_
            ( handlers ref portRef <>
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "-select")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cx"        $= (px <> "px")
              , "cy"        $= (py <> "px")
              ]
            ) mempty

argumentConstructor_ :: IsRef r => r -> NodeLoc -> Int -> Bool -> Bool -> Bool -> ReactElementM ViewEventHandler ()
argumentConstructor_ ref nl numOfPorts isConnectionSource hasAlias hasSelf = do
    let portRef   = toAnyPortRef nl $ InPortId' [Arg numOfPorts]
        offsetY   = argumentConstructorOffsetY $ max 0 $ numOfPorts - if hasAlias then 0 else 1
        highlight = if isConnectionSource then ["port--highlighted"] else []
    g_
        [ "key"       $= "argument-constructor"
        , "className" $= Style.prefixFromList (["port", "port--i", "port--i--constructor"] <> highlight)
        ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= "shape"
            , "style"     @= Aeson.object [ "cy" Aeson..= (show offsetY <> "px") ]
            , "r"         $= jsShow2 3
            ] mempty
        circle_
            ( handlers ref portRef <>
                [ "className" $= Style.prefix "port__select"
                , "key"       $= "select"
                , "r"         $= jsShow2 (lineHeight/1.5)
                , "style"     @= Aeson.object [ "cy" Aeson..= (show offsetY <> "px") ]
                ]
            ) mempty
