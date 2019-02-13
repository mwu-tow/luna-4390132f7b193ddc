module Data.Abstract_OLD where

import Control.Lens



type family Abstract a

class HasAbstract a where abstract :: Lens' a (Abstract a)
                          default abstract :: IsAbstract a => Lens' a (Abstract a)
                          abstract = abstracted

class IsAbstract a where abstracted :: Iso' a (Abstract a)
