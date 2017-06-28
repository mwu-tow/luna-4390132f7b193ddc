{-# LANGUAGE OverloadedStrings #-}

module Mock.NodeSearcher where

import qualified Data.Map                    as Map
import           Text.ScopeSearcher.Item

items = _items group

group  = Group $ Map.fromList  [
         ("id"             , Element)
       , ("const"          , Element)
       , ("app"            , Element)
       , ("comp"           , Element)
       , ("flip"           , Element)
       , ("empty"          , Element)
       , ("singleton"      , Element)
       , ("switch"         , Element)
       , ("readFile"       , Element)
       , ("mean"           , Element)
       , ("differences"    , Element)
       , ("histogram"      , Element)
       , ("primes"         , Element)
       , ("List"           , Group $ Map.fromList [
                                             ("+"          , Element)
                                           , ("append"     , Element)
                                           , ("prepend"    , Element)
                                           , ("length"     , Element)
                                           , ("reverse"    , Element)
                                           , ("take"       , Element)
                                           , ("drop"       , Element)
                                           , ("sort"       , Element)

                                           , ("fold"       , Element)
                                           , ("map"        , Element)
                                           , ("zip"        , Element)
                                           , ("filter"     , Element)
                                           ])
       , ("Int"            , Group $ Map.fromList [
                                             ("=="         , Element)
                                           , ("/="         , Element)
                                           , ("<"          , Element)
                                           , ("<="         , Element)
                                           , (">"          , Element)
                                           , (">="         , Element)
                                           , ("min"        , Element)
                                           , ("max"        , Element)

                                           , ("+"          , Element)
                                           , ("*"          , Element)
                                           , ("-"          , Element)
                                           , ("/"          , Element)
                                           , ("%"          , Element)
                                           , ("^"          , Element)

                                           , ("negate"     , Element)
                                           , ("abs"        , Element)
                                           , ("signum"     , Element)

                                           , ("pred"       , Element)
                                           , ("succ"       , Element)
                                           , ("even"       , Element)
                                           , ("odd"        , Element)

                                           , ("gcd"        , Element)
                                           , ("lcm"        , Element)

                                           , ("times"      , Element)
                                           , ("upto"       , Element)

                                           , ("toDouble"   , Element)
                                           , ("toString"   , Element)
                                           ])
       , ("Double"         , Group $ Map.fromList [
                                             ("=="         , Element)
                                           , ("/="         , Element)
                                           , ("<"          , Element)
                                           , ("<="         , Element)
                                           , (">"          , Element)
                                           , (">="         , Element)
                                           , ("min"        , Element)
                                           , ("max"        , Element)

                                           , ("+"          , Element)
                                           , ("*"          , Element)
                                           , ("-"          , Element)
                                           , ("/"          , Element)
                                           , ("**"         , Element)

                                           , ("negate"     , Element)
                                           , ("abs"        , Element)
                                           , ("signum"     , Element)

                                           , ("round"      , Element)
                                           , ("ceiling"    , Element)
                                           , ("floor"      , Element)

                                           , ("exp"        , Element)
                                           , ("log"        , Element)
                                           , ("sqrt"       , Element)

                                           , ("sin"        , Element)
                                           , ("cos"        , Element)
                                           , ("tan"        , Element)
                                           , ("asin"       , Element)
                                           , ("acos"       , Element)
                                           , ("atan"       , Element)
                                           , ("sinh"       , Element)
                                           , ("cosh"       , Element)
                                           , ("tanh"       , Element)
                                           , ("asinh"      , Element)
                                           , ("acosh"      , Element)
                                           , ("atanh"      , Element)

                                           , ("toString"   , Element)
                                           ])
       , ("Bool",            Group $ Map.fromList [
                                             ("=="         , Element)
                                           , ("/="         , Element)
                                           , ("<"          , Element)
                                           , ("<="         , Element)
                                           , (">"          , Element)
                                           , (">="         , Element)
                                           , ("min"        , Element)
                                           , ("max"        , Element)

                                           , ("&&"         , Element)
                                           , ("||"         , Element)
                                           , ("not"        , Element)

                                           , ("toString"   , Element)
                                           ])
       , ("String"         , Group $ Map.fromList [
                                             ("=="         , Element)
                                           , ("/="         , Element)
                                           , ("<"          , Element)
                                           , ("<="         , Element)
                                           , (">"          , Element)
                                           , (">="         , Element)
                                           , ("min"        , Element)
                                           , ("max"        , Element)
                                           , ("+"          , Element)
                                           , ("length"     , Element)
                                           , ("reverse"    , Element)
                                           , ("take"       , Element)
                                           , ("drop"       , Element)
                                           , ("words"      , Element)
                                           , ("lines"      , Element)
                                           , ("join"       , Element)
                                           , ("toString"   , Element)
                                           ])
       ]
