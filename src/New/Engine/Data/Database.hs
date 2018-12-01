{-# LANGUAGE Strict #-}
module New.Engine.Data.Database where

import Prologue hiding (Index)

import qualified New.Engine.Data.Tree as Tree

import New.Engine.Data.Tree (Node)
import New.Engine.Data.Index (Index)
import Data.Map.Strict (Map)
import Control.Lens (Getter, to)
import Data.Text    (Text)



--------------------------
-- === SearcherData === --
--------------------------


-- === Definition === --

class Eq a => SearcherData a where
    text           :: Getter a Text
    calculateScore :: Int -> a -> Int

instance SearcherData Text where
    text               = to id
    calculateScore p _ = p


----------------------
-- === Database === --
----------------------


-- === Definition === --

data Database a = Database
    { _hints :: Map Index [a]
    , _tree  :: Tree.Root
    } deriving (Eq, Generic, Show)

-- insert :: SearcherData a => a -> Database a -> Database a
-- insert hint database = do
--     let root = database ^. tree
--         txt  = hint ^. text
--         insert txt