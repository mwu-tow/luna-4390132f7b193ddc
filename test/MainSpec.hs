{-# LANGUAGE OverloadedStrings #-}
module MainSpec (spec) where

import Prologue
import Test.Hspec
import LunaStudio.Data.NodeSearcher (ModuleHints (ModuleHints), ImportName)
import qualified Data.Map as Map
import Data.Map (Map)



mockBase :: ModuleHints
mockBase = ModuleHints ["if_then_else", "seq", "switch", "id", "everyWithState", "newMVar", "fork", "streamFrom", "delayAsync", "every", "pi", "const", "when", "unless", "print"]
                       $ Map.fromList [ ("Binary", ["+", "equals", "==", "toText", "shortRep", "length", "toBinary"])
                                      , ("Bool", ["equals", "toText", "toJSON", "shortRep", "and", "or", "not"])
                                      , ("Complex", ["*", "+", "shortRep", "modulus", "real", "imaginary"])
                                      , ("Either", ["equals", "toText", "toJSON", "shortRep", "map", "flatMap", "either", "toMaybe", "isLeft", "isRight", "left", "right"])
                                      , ("Int", ["-", "*", "%", "+", "/", "negate", ">", "<", "equals", "==", "pred", "toText", "toJSON", "shortRep", "toReal", "miliseconds", "seconds", "minutes", "succ", "upto"])
                                      , ("JSON", ["sum", "toText", "toJSON", "shortRep", "toBinary", "safeParse", "parse", "fromObject", "fromText", "render", "lookup", "toReal", "toList", "lookupReal", "lookupText", "at", "map", "filter", "each", "average"])
                                      , ("List", ["+", "equals", "sum", "head", "toText", "toJSON", "shortRep", "isEmpty", "length", "at", "map", "filter", "each", "average", "tail", "prepend", "sequence", "take", "drop", "fold", "prependAll", "intersperse", "_merge", "sort", "_prefixes", "prefixes", "zipWith", "zip", "_all", "_any", "collect", "makeText", "concat", "flatMap", "contains"])
                                      , ("MVar", ["take", "read", "put"])
                                      , ("Map", ["equals", "toJSON", "isEmpty", "lookup", "toList", "map", "empty", "size", "insert"])
                                      , ("Maybe", ["equals", "toText", "toJSON", "shortRep", "toList", "map", "each", "flatMap", "fromJust", "fromMaybe", "maybe", "isJust", "isNothing", "join"])
                                      , ("MsgPack", ["equals", "sum", "toText", "toJSON", "shortRep", "toBinary", "lookup", "toReal", "toList", "lookupText", "at", "map", "filter", "each", "average", "_lookup_list_key", "hasKey", "hasText", "safeLookupText"])
                                      , ("None", ["toText", "toJSON", "shortRep"])
                                      , ("Real", ["-", "*", "+", "/", "negate", ">", "<", "equals", "==", "toText", "toJSON", "shortRep", "round", "sin", "cos", "tan"])
                                      , ("Stream", ["head", "map", "each", "average", "tail", "take", "drop", "fold", "collect", "isStream", "foldFrom", "consume", "consumeWhile", "takeWhile", "dropWhile", "eval", "rateLimit"])
                                      , ("Text", ["+", ">", "<", "equals", "==", "toText", "toJSON", "shortRep", "isEmpty", "length", "hasPrefix", "isPrefixOf", "characters", "words", "lines", "lowercase", "uppercase", "reverse", "escapeJSON", "toBinary"])
                                      , ("Tuple2", ["equals", "toText", "toJSON", "shortRep", "map", "first", "second"])
                                      , ("Tuple3", ["equals", "toText", "toJSON", "shortRep", "map", "first", "second", "third"])
                                      , ("Tuple4", ["equals", "toText", "toJSON", "shortRep", "map", "first", "second", "third", "fourth"])
                                      ]
mockGeo :: ModuleHints
mockGeo = ModuleHints [] $ Map.fromList [ ("GeoJSONFeature", ["toJSON", "properties", "geometry"])
                                        , ("GeoJSONFeatureCollection", ["toJSON", "features"])
                                        ]

mockGraphics2D :: ModuleHints
mockGraphics2D = ModuleHints ["path", "translationTrans", "rotationTrans", "identityTrans", "point", "lpoint", "rpoint", "lrpoint", "circle", "rectangle", "emptyGeo"]
                             $ Map.fromList [ ("AffineTransformation", ["a", "b", "*", "+", "c", "d", "toJSON", "toList", "tx", "ty"])
                                            , ("Boolean", ["toJSON", "toSVGDefs", "type", "operands"])
                                            , ("BooleanOperation", ["toJSON"])
                                            , ("ControlPoint", ["toJSON", "point", "leftHandle", "rightHandle"])
                                            , ("Geo", ["-", "*", "+", "toJSON", "transform", "translate", "rotate", "toSVGDefs", "toSVG", "transformation", "definition"])
                                            , ("GeoDef", ["toJSON", "toSVGDefs"])
                                            , ("Point", ["x", "y", "toJSON"])
                                            , ("Shape", ["toJSON", "toSVGDefs"])
                                            ]

mockHttp :: ModuleHints
mockHttp = ModuleHints ["defaultHttpRequest", "emptyHttpBody"]
                       $ Map.fromList [ ("Http", ["put", "get", "post", "delete", "getBinary", "getJSON"])
                                      , ("HttpMethod", ["toText"])
                                      , ("HttpRequest", ["toText", "shortRep", "uri", "headers", "auth", "oauth1", "params", "body", "method", "setMethod", "setBody", "setUri", "addHeader", "setBasicAuth", "setParam", "setOAuth1", "setOAuth2", "perform"])
                                      , ("HttpResponse", ["json", "stream", "toText", "shortRep", "body", "successful", "text", "responseCode", "getChunk"])
                                      , ("HttpSimpleBody", ["toText", "shortRep", "toBinary", "addValue", "values"])
                                      ]

mockOAuth :: ModuleHints
mockOAuth = ModuleHints []
                        $ Map.fromList [ ("OAuth1Data", ["toText", "shortRep", "clientKey", "clientSecret", "oauthToken", "oauthTokenSecret"])
                                       , ("OAuth2", ["postRequest", "fetchAccessToken", "invalidateToken"])
                                       , ("OAuth2Data", ["toText", "shortRep", "clientSecret", "clientId", "accessTokenEndpoint", "invalidateTokenEndpoint", "callback"])
                                       ]

mockSystem :: ModuleHints
mockSystem = ModuleHints ["withForkWait"]
                         $ Map.fromList [ ("BufferMode", [])
                                        , ("Command", ["args", "command", "run", "processDescription", "runWithInput", "runWithStream", "execute"])
                                        , ("ExitCode", ["toText", "shortRep", "toInt", "exitSuccess", "exitFailure"])
                                        , ("FileHandle", ["setBuffering", "isOpen", "isClosed", "close", "flush", "getContents", "getLine", "putText", "putLine", "toStream"])
                                        , ("PipeRequest", [])
                                        , ("Process", ["stdin", "stdout", "stderr", "wait", "handle"])
                                        , ("ProcessDescription", ["args", "stdin", "stdout", "stderr", "command", "setCommand", "setArgs", "setStdin", "setStdout", "setStderr", "run"])
                                        , ("ProcessHandle", ["wait"])
                                        ]

mockTime :: ModuleHints
mockTime = ModuleHints []
                       $ Map.fromList [ ("Time", [">", "<", "equals", "toText", "shortRep", "safeParse", "parse", "diff", "add", "sub", "now", "timeOfDay", "defaultFormat", "safeParseFmt", "parseFmt"])
                                      , ("TimeInterval", ["-", "+", ">", "<", "equals", "toText", "shortRep", "toReal", "toInt", "toSeconds", "toMiliseconds", "toMicroseconds", "add", "sub", "before", "from", "ago", "fromNow"])
                                      ]

mockWebSockets :: ModuleHints
mockWebSockets = ModuleHints []
                             $ Map.fromList [ ("WSConnection", ["stream", "read", "close", "write", "writeBinary"])
                                            , ("WebSocket", ["create"])
                                            , ("WebSocketInstance", ["path", "port", "secure", "host", "setHost", "setPath", "setPort", "setSecure", "connect"])
                                            ]

mockXML :: ModuleHints
mockXML = ModuleHints [] $ Map.fromList [("XNode", ["render", "setAttr"])]

mockImports :: Map ImportName ModuleHints
mockImports = Map.fromList [ ("Std.Base", mockBase)
                           , ("Std.Geo", mockGeo)
                           , ("Std.Graphics2D", mockGraphics2D)
                           , ("Std.Http", mockHttp)
                           , ("Std.OAuth", mockOAuth)
                           , ("Std.System", mockSystem)
                           , ("Std.Time", mockTime)
                           , ("Std.WebSockets", mockWebSockets)
                           , ("Std.XML", mockXML)
                           ]


spec :: Spec
spec = do
    describe "scoring function" $ do
        it "assings proper score to dupa" $ do
            1 `shouldBe` 1