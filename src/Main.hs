{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map (Map)
import qualified Data.Map                     as Map
import qualified Data.Text                    as Text
import           FuzzyText
import           LunaStudio.Data.NodeSearcher (ClassHints (ClassHints), ImportName, ModuleHints (ModuleHints))
import           Prologue


mockBase :: ModuleHints
mockBase = ModuleHints ["if_then_else", "seq", "switch", "id", "everyWithState", "newMVar", "fork", "streamFrom", "delayAsync", "every", "pi", "const", "when", "unless", "print"]
                       $ Map.fromList [ ("Binary",  ClassHints def ["+", "equals", "==", "toText", "shortRep", "length", "toBinary"])
                                      , ("Bool",    ClassHints def ["equals", "toText", "toJSON", "shortRep", "and", "or", "not"])
                                      , ("Complex", ClassHints def ["*", "+", "shortRep", "modulus", "real", "imaginary"])
                                      , ("Either",  ClassHints def ["equals", "toText", "toJSON", "shortRep", "map", "flatMap", "either", "toMaybe", "isLeft", "isRight", "left", "right"])
                                      , ("Int",     ClassHints def ["-", "*", "%", "+", "/", "negate", ">", "<", "equals", "==", "pred", "toText", "toJSON", "shortRep", "toReal", "miliseconds", "seconds", "minutes", "succ", "upto"])
                                      , ("JSON",    ClassHints def ["sum", "toText", "toJSON", "shortRep", "toBinary", "safeParse", "parse", "fromObject", "fromText", "render", "lookup", "toReal", "toList", "lookupReal", "lookupText", "at", "map", "filter", "each", "average"])
                                      , ("List",    ClassHints def ["+", "equals", "sum", "head", "toText", "toJSON", "shortRep", "isEmpty", "length", "at", "map", "filter", "each", "average", "tail", "prepend", "sequence", "take", "drop", "fold", "prependAll", "intersperse", "_merge", "sort", "_prefixes", "prefixes", "zipWith", "zip", "_all", "_any", "collect", "makeText", "concat", "flatMap", "contains"])
                                      , ("MVar",    ClassHints def ["take", "read", "put"])
                                      , ("Map",     ClassHints def ["equals", "toJSON", "isEmpty", "lookup", "toList", "map", "empty", "size", "insert"])
                                      , ("Maybe",   ClassHints def ["equals", "toText", "toJSON", "shortRep", "toList", "map", "each", "flatMap", "fromJust", "fromMaybe", "maybe", "isJust", "isNothing", "join"])
                                      , ("MsgPack", ClassHints def ["equals", "sum", "toText", "toJSON", "shortRep", "toBinary", "lookup", "toReal", "toList", "lookupText", "at", "map", "filter", "each", "average", "_lookup_list_key", "hasKey", "hasText", "safeLookupText"])
                                      , ("None",    ClassHints def ["toText", "toJSON", "shortRep"])
                                      , ("Real",    ClassHints def ["-", "*", "+", "/", "negate", ">", "<", "equals", "==", "toText", "toJSON", "shortRep", "round", "sin", "cos", "tan"])
                                      , ("Stream",  ClassHints def ["head", "map", "each", "average", "tail", "take", "drop", "fold", "collect", "isStream", "foldFrom", "consume", "consumeWhile", "takeWhile", "dropWhile", "eval", "rateLimit"])
                                      , ("Text",    ClassHints def ["+", ">", "<", "equals", "==", "toText", "toJSON", "shortRep", "isEmpty", "length", "hasPrefix", "isPrefixOf", "characters", "words", "lines", "lowercase", "uppercase", "reverse", "escapeJSON", "toBinary"])
                                      , ("Tuple2",  ClassHints def ["equals", "toText", "toJSON", "shortRep", "map", "first", "second"])
                                      , ("Tuple3",  ClassHints def ["equals", "toText", "toJSON", "shortRep", "map", "first", "second", "third"])
                                      , ("Tuple4",  ClassHints def ["equals", "toText", "toJSON", "shortRep", "map", "first", "second", "third", "fourth"])
                                      ]
mockGeo :: ModuleHints
mockGeo = ModuleHints [] $ Map.fromList [ ("GeoJSONFeature",           ClassHints def ["toJSON", "properties", "geometry"])
                                        , ("GeoJSONFeatureCollection", ClassHints def ["toJSON", "features"])
                                        ]

mockGraphics2D :: ModuleHints
mockGraphics2D = ModuleHints ["path", "translationTrans", "rotationTrans", "identityTrans", "point", "lpoint", "rpoint", "lrpoint", "circle", "rectangle", "emptyGeo"]
                             $ Map.fromList [ ("AffineTransformation", ClassHints def ["a", "b", "*", "+", "c", "d", "toJSON", "toList", "tx", "ty"])
                                            , ("Boolean",              ClassHints def ["toJSON", "toSVGDefs", "type", "operands"])
                                            , ("BooleanOperation",     ClassHints def ["toJSON"])
                                            , ("ControlPoint",         ClassHints def ["toJSON", "point", "leftHandle", "rightHandle"])
                                            , ("Geo",                  ClassHints def ["-", "*", "+", "toJSON", "transform", "translate", "rotate", "toSVGDefs", "toSVG", "transformation", "definition"])
                                            , ("GeoDef",               ClassHints def ["toJSON", "toSVGDefs"])
                                            , ("Point",                ClassHints def ["x", "y", "toJSON"])
                                            , ("Shape",                ClassHints def ["toJSON", "toSVGDefs"])
                                            ]

mockHttp :: ModuleHints
mockHttp = ModuleHints ["defaultHttpRequest", "emptyHttpBody"]
                       $ Map.fromList [ ("Http",           ClassHints def ["put", "get", "post", "delete", "getBinary", "getJSON"])
                                      , ("HttpMethod",     ClassHints def ["toText"])
                                      , ("HttpRequest",    ClassHints def ["toText", "shortRep", "uri", "headers", "auth", "oauth1", "params", "body", "method", "setMethod", "setBody", "setUri", "addHeader", "setBasicAuth", "setParam", "setOAuth1", "setOAuth2", "perform"])
                                      , ("HttpResponse",   ClassHints def ["json", "stream", "toText", "shortRep", "body", "successful", "text", "responseCode", "getChunk"])
                                      , ("HttpSimpleBody", ClassHints def ["toText", "shortRep", "toBinary", "addValue", "values"])
                                      ]

mockOAuth :: ModuleHints
mockOAuth = ModuleHints []
                        $ Map.fromList [ ("OAuth1Data", ClassHints def ["toText", "shortRep", "clientKey", "clientSecret", "oauthToken", "oauthTokenSecret"])
                                       , ("OAuth2",     ClassHints def ["postRequest", "fetchAccessToken", "invalidateToken"])
                                       , ("OAuth2Data", ClassHints def ["toText", "shortRep", "clientSecret", "clientId", "accessTokenEndpoint", "invalidateTokenEndpoint", "callback"])
                                       ]

mockSystem :: ModuleHints
mockSystem = ModuleHints ["withForkWait"]
                         $ Map.fromList [ ("BufferMode",         ClassHints def [])
                                        , ("Command",            ClassHints def ["args", "command", "run", "processDescription", "runWithInput", "runWithStream", "execute"])
                                        , ("ExitCode",           ClassHints def ["toText", "shortRep", "toInt", "exitSuccess", "exitFailure"])
                                        , ("FileHandle",         ClassHints def ["setBuffering", "isOpen", "isClosed", "close", "flush", "getContents", "getLine", "putText", "putLine", "toStream"])
                                        , ("PipeRequest",        ClassHints def [])
                                        , ("Process",            ClassHints def ["stdin", "stdout", "stderr", "wait", "handle"])
                                        , ("ProcessDescription", ClassHints def ["args", "stdin", "stdout", "stderr", "command", "setCommand", "setArgs", "setStdin", "setStdout", "setStderr", "run"])
                                        , ("ProcessHandle",      ClassHints def ["wait"])
                                        ]

mockTime :: ModuleHints
mockTime = ModuleHints []
                       $ Map.fromList [ ("Time",         ClassHints def [">", "<", "equals", "toText", "shortRep", "safeParse", "parse", "diff", "add", "sub", "now", "timeOfDay", "defaultFormat", "safeParseFmt", "parseFmt"])
                                      , ("TimeInterval", ClassHints def ["-", "+", ">", "<", "equals", "toText", "shortRep", "toReal", "toInt", "toSeconds", "toMiliseconds", "toMicroseconds", "add", "sub", "before", "from", "ago", "fromNow"])
                                      ]

mockWebSockets :: ModuleHints
mockWebSockets = ModuleHints []
                             $ Map.fromList [ ("WSConnection",      ClassHints def ["stream", "read", "close", "write", "writeBinary"])
                                            , ("WebSocket",         ClassHints def ["create"])
                                            , ("WebSocketInstance", ClassHints def ["path", "port", "secure", "host", "setHost", "setPath", "setPort", "setSecure", "connect"])
                                            ]

mockXML :: ModuleHints
mockXML = ModuleHints [] $ Map.fromList [("XNode", ClassHints def ["render", "setAttr"])]

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


main :: IO ()
main = forever $ do
    line <- getLine
    print line
    pprint $ take 5 $ processEntries (Text.pack line) $ toEntries mockImports True

