{-# LANGUAGE UndecidableInstances #-}

module Type.Error (module Type.Error, module X) where

import GHC.TypeLits as X (ErrorMessage (ShowType, (:<>:), (:$$:)), TypeError)
import GHC.TypeLits      (ErrorMessage (Text))


-- === ErrMsg === --

type ErrMsg = 'Text


-- === Assertions === --
class                     TypeErrorIf (ok :: Bool) (err :: ErrorMessage)
instance                  TypeErrorIf 'True  err
instance TypeError err => TypeErrorIf 'False err

type TypeAssert ok = TypeErrorIf ok (ErrMsg "Assertion failed.")


-- === Formatters === --

type Sentence a     = a :<>: ErrMsg "."
type Ticked   a     = Between' "`" a
type Parensed a     = Between "(" ")" a
type Between  l r a = ErrMsg l :<>: a :<>: ErrMsg r
type Between' s   a = Between s s a
type a :</>: b      = a :<>: ErrMsg " " :<>: b
