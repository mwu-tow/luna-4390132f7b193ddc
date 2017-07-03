{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ColorizedExpression where

import           Common.Prelude
import           Data.List.Split        (wordsBy)
import           JS.Lexer               (lunaClass)
import qualified Luna.Syntax.Text.Lexer as Lexer
import           React.Flux


colorizedExpression_ :: Text -> ReactElementM ViewEventHandler ()
colorizedExpression_ expr = do
    let classes t = map ("syntax--" ++) $ "source" : "luna" : concatMap (wordsBy (=='.')) (mapMaybe lunaClass t)
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
