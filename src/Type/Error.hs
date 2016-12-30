{-# LANGUAGE UndecidableInstances #-}

module Type.Error (module Type.Error, module X) where

import GHC.TypeLits as X (ErrorMessage (ShowType, (:<>:), (:$$:)), TypeError)
import GHC.TypeLits      (ErrorMessage (Text))


type ErrMsg = 'Text


-- === Assertions === --
class                     Assert (ok :: Bool) (err :: ErrorMessage)
instance                  Assert 'True  err
instance TypeError err => Assert 'False err

type Assert' ok = Assert ok (ErrMsg "Assertion failed.")


-- === Formatters === --

type Sentence a     = a :<>: ErrMsg "."
type Ticked   a     = Between' "`" a
type Parensed a     = Between "(" ")" a
type Between  l r a = ErrMsg l :<>: a :<>: ErrMsg r
type Between' s   a = Between s s a
type a :</>: b  = a :<>: ErrMsg " " :<>: b
