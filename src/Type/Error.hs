{-# LANGUAGE UndecidableInstances #-}

module Type.Error where

import GHC.TypeLits (ErrorMessage(..), TypeError)


-- === Assertions === --
class                     Assert (ok :: Bool) (err :: ErrorMessage)
instance                  Assert 'True  err
instance TypeError err => Assert 'False err


-- === Formatters === --

type Ticked a = 'Text "`" :<>: a :<>: 'Text "`"
type a :</>: b = a :<>: 'Text " " :<>: b
