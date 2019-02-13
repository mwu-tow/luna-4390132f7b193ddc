module Type.Hidden where

import Prelude
import Unsafe.Coerce

class Hidden t where
    hide   :: a -> t
    reveal :: t -> a

data Any where
    Any :: a -> Any

instance Show Any where
    show _ = "Hidden"

instance Hidden Any where
    hide              = Any
    reveal (Any a) = unsafeCoerce a


