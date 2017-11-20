{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Plane where

import           Common.Prelude
import           JS.Scene                    (planeCanvasId)
import qualified NodeEditor.React.View.Style as Style
import           React.Flux

planeMonads_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeMonads_ =
    svg_
        [ "className" $= Style.prefixFromList [ "svg-plane", "svg-plane--monads" ]
        , "key"       $= "svgPlaneMonads"
        ] . g_
            [ "className" $= Style.prefixFromList [ "plane", "plane--monads", "camera-transform" ]
            , "key"       $= "monads"
            ]

planeConnections_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeConnections_ =
    svg_
        [ "className" $= Style.prefixFromList [ "svg-plane", "svg-plane--connections" ]
        , "key"       $= "svgPlaneConnections"
        ] . g_
            [ "className" $= Style.prefixFromList [ "plane", "plane--connections", "camera-transform" ]
            , "key"       $= "connections"
            ]

planeNewConnection_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeNewConnection_ =
    svg_
        [ "className" $= Style.prefixFromList [ "svg-plane", "svg-plane--new-connection" ]
        , "key"       $= "svgPlaneNewConnection"
        ] . g_
            [ "className" $= Style.prefixFromList [ "plane", "plane--new-connection", "camera-transform" ]
            , "key"       $= "newConnection"
            ]

planeNodes_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeNodes_ =
    div_
        [ "className" $= Style.prefixFromList [ "plane", "plane--nodes" ]
        , "key"       $= "nodes"
        ]

planeCanvas_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeCanvas_ =
    canvas_
        [ "className" $= Style.prefixFromList [ "plane", "plane--canvas", "hide" ]
        , "key"       $= "canvas"
        , "id"        $= planeCanvasId
        , onDoubleClick $ \e m -> [stopPropagation e]
        ]
