{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ColorizedExpression where

import           Common.Prelude
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Luna.Syntax.Text.Lexer as Lexer
import           React.Flux


lunaClasses :: Map String [Char]
lunaClasses = Map.fromList [
        -- "Layout"      ,
        -- "BOF"         ,
        -- "EOF"         ,
        -- "EOL"         ,
        -- "Terminator"  ,
        -- "BlockStart"  ,
        -- "Block"       ,
        -- "Group"       ,
          ("Marker"      , "marker")

        --, ( "Ident"       ,)
        , ("Var"         , "variable")
        , ("Cons"        , "constant")
        , ("Wildcard"    , "variable")

        , ("Keyword"     , "keyword")
        , ("KwAll"       , "keyword.control")
        , ("KwCase"      , "keyword.control")
        , ("KwClass"     , "meta.class")
        , ("KwDef"       , "meta.class")
        , ("KwImport"    , "meta.import")
        , ("KwOf"        , "keyword.control")

        , ("Operator"    , "keyword.operator")
        , ("Modifier"    , "keyword.other.special-method")
        , ("Accessor"    , "keyword.other.special-method")
        --, ( "Assignment"  ,)
        --, ( "TypeApp"     ,)
        --, ( "Merge"       ,)
        --, ( "Range"       ,)
        --, ( "Anything"    ,)

        , ("Literal"     , "constant")
        , ("Number"      , "constant.numeric")
        --, ( "Quote"       ,)
        , ("Str"         , "string")
        , ("StrEsc"      , "constant.character.escape")
        , ("List"        , "storage")
        , ("StrWrongEsc" , "invalid.illegal")

        --, ( "Control"     ,)
        , ("Disabled"    , "disabled")

        , ("Comment"     , "comment")
        --, ( "Doc"         ,)

        --, ( "Unknown"     ,)
        ]

colorizedExpression_ :: Text -> ReactElementM ViewEventHandler ()
colorizedExpression_ expr = do
    let classes t = map ("syntax--" ++) $ "source" : "luna" : mapMaybe (flip Map.lookup lunaClasses) t
        tags token = [ (fromIntegral $ unwrap $ token ^. Lexer.guiSpan, classes $ Lexer.getTags token)
                     , (fromIntegral $ unwrap $ token ^. Lexer.guiOffset, [])
                     ]
        lexerData = concatMap tags $ Lexer.runGUILexer $ convert expr
    div_ $ spans_ lexerData $ convert expr

spans_ :: [(Int, [String])] -> String -> ReactElementM ViewEventHandler ()
spans_ []              _    = span_ [] $ elemString ""
spans_ _               ""   = span_ [] $ elemString ""
spans_ ((i, cls):rest) expr = span_ ["className" $= fromString (unwords cls)] (elemString current) >> spans_ rest remaining where
    (current, remaining) = splitAt i expr
