{-# LANGUAGE OverloadedStrings #-}
module App (myApp) where

import React.Flux
import NavStore
import Dispatcher
import PageViews

-- | Each page rendered by the application becomes an instance of this structure.
-- I define and fill this here inside App.hs instead of in each individual page view so
-- that the navigation definition can control how the pages are displayed and organized (dropdowns,
-- categories, etc.) in the menu.
data Page = Page
  { pageId :: NavPageId
  , pageTitle :: ReactElementM ViewEventHandler ()
    -- ^ the title showed in the sidebar menu
  , pageContent :: ReactView ()
    -- ^ the content of the page
  }

-- | Convert a page id to a page
pageFor :: ReactStore NavState -> NavPageId -> Page
pageFor navStore Page1 = Page Page1 "Page 1" $ page1 navStore
pageFor navStore Page2 = Page Page2 "Page 2" $ page2 navStore
pageFor navStore Page3 = Page Page3 "Page 3" $ page3 navStore

-- | A single menu item entry in the sidebar.
menuItem_ :: ReactStore NavState -> NavPageId -> NavPageId -> ReactElementM ViewEventHandler ()
menuItem_ navStore curPageId linkPageId =
    let linkPage = pageFor navStore linkPageId
    in
    li_ [classNames [("pure-menu-item", True), ("pure-menu-selected", curPageId == pageId linkPage)]] $
        a_ ["className" $= "pure-menu-link", onClick $ \_ _ -> changePageTo navStore $ pageId linkPage] $
            pageTitle linkPage

-- | The navigation menu
navMenu_ :: ReactStore NavState -> NavPageId -> ReactElementM ViewEventHandler ()
navMenu_ navStore curPageId =
    cldiv_ "pure-menu" $ do
        span_ ["className" $= "pure-menu-heading"] "My Brand"
        ul_ ["className" $= "pure-menu-list"] $
            mapM_ (menuItem_ navStore curPageId) allPageIds

-- | The entire layout of the app, consisting of the menu and the main content section.
myApp :: ReactStore NavState -> ReactView ()
myApp navStore = defineControllerView "my application" navStore $ \navState () -> do
    div_ ["id" $= "layout", classNames [("active", sideMenuOpen navState)]] $ do
        a_ ["id" $= "menuLink"
           , classNames [("menu-link", True), ("active", sideMenuOpen navState)]
           , onClick $ \_ _ -> [SomeStoreAction navStore ToggleSideMenu]
           ] $ span_ mempty
        div_ ["id" $= "menu", classNames [("active", sideMenuOpen navState)]] $
            navMenu_ navStore $ currentPageId navState
        div_ ["id" $= "main"] $
            view (pageContent $ pageFor navStore $ currentPageId navState) () mempty
