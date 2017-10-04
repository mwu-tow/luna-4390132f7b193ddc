{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Breadcrumbs where

import           Common.Prelude
import           LunaStudio.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)



data Event = Enter (Breadcrumb BreadcrumbItem)
           | Exit
            deriving (Show, Generic, NFData, Typeable)
