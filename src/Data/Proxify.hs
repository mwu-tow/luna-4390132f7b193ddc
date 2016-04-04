module Data.Proxify (module Data.Proxify, module X) where

import Data.Proxy as X


-- === Utils === --

type family Proxified a where
    Proxified (Proxy a) = a
    Proxified a         = a

proxify :: a -> Proxy (Proxified a)
proxify _ = Proxy
