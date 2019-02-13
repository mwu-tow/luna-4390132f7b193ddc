
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Type.Operators where

import Data.Typeable
import Prelude
import GHC.Exts (Constraint)
import GHC.TypeLits
import Type.Bool
import Type.Container
import Type.Wrapped



-- | The `$$` operator is just like `$` one but with even lower precedence level.
--   Unlike value-level `$`, the type-level one has precedence level of `infixr 1`
--   in order to be used in function arguments, like `edge :: Node $ Source a -> Node $ Target a -> a`.
--   The `$$` operator has higher precedence than `->`, so the above expression would not be valid when using it.

infixr 0 $$
type f $$ a = f a

infixr 1 $
type f $ a = f a

infixl 1 &
type a & f = f a
