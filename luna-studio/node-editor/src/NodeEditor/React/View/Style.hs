{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Style where

import           Common.Prelude
import           Data.List      (intercalate)
import           React.Flux     as React

prefix :: JSString -> JSString
prefix a = (<>) "luna-" a

prefixFromList :: [String] -> JSString
prefixFromList a = fromString $ intercalate " " $ map ((<>) "luna-") a

blurBackground_ :: ReactElementM ViewEventHandler ()
blurBackground_ = div_
    [ "key"       $= "blurBackground"
    , "className" $= prefix "blur"
    ] mempty

selectionMark_ :: ReactElementM ViewEventHandler ()
selectionMark_ = div_
    [ "key"       $= "selectionMark"
    , "className" $= prefix "selection"
    ] mempty

errorMark_ :: ReactElementM ViewEventHandler ()
errorMark_ = div_
    [ "key"       $= "errorMark"
    , "className" $= prefix "error"
    ] $ div_ mempty

plainRect_ :: JSString -> Double -> Double -> Double -> Double -> ReactElementM ViewEventHandler ()
plainRect_ key w h x y = React.viewWithSKey plainRect key (w, h, x, y) mempty

plainRect :: ReactView (Double, Double, Double, Double)
plainRect = React.defineView "plain-rect" $ \(w, h, x, y) -> do
    rect_ [ "width"  $= fromString (show w)
          , "height" $= fromString (show h)
          , "x"      $= fromString (show x)
          , "y"      $= fromString (show y)
          ] mempty

plainPath_ :: JSString -> JSString -> ReactElementM ViewEventHandler ()
plainPath_ c d = React.viewWithSKey plainPath "plain-path" (c, d) mempty

plainPath :: ReactView (JSString, JSString)
plainPath = React.defineView "plain-path" $ \(c, d) ->
    path_ [ "className" $= c, "d" $= d ] mempty
