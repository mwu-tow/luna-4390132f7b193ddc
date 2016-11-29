{-# LANGUAGE UndecidableInstances #-}

module Type.Error (module Type.Error, module X) where

import GHC.TypeLits as X (ErrorMessage (ShowType, (:<>:), (:$$:)), TypeError)
import GHC.TypeLits      (ErrorMessage (Text))


type ErrMsg = 'Text


-- === Assertions === --
class                     Assert (ok :: Bool) (err :: ErrorMessage)
instance                  Assert 'True  err
instance TypeError err => Assert 'False err


-- === Formatters === --

type Sentence a = a :<>: ErrMsg "."
type Ticked   a = ErrMsg "`" :<>: a :<>: ErrMsg "`"
type a :</>: b  = a :<>: ErrMsg " " :<>: b
