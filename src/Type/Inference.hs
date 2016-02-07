module Type.Inference where

import Prelude


-- === Definitions ===

-- | The Inferable type class describes a monad with a functional dependency on the given type.
--   It allows for writing polymorphic code and ensuring Haskell that the type will be resolved while
--   evaluating the monadic stack.
class Monad m => Inferable a t m | a m -> t where infer_ :: a -> t -> m ()


-- === Utils === --

infer :: Inferable a t m => a -> t -> m t
infer a t = t <$ infer_ a t

inferM :: Inferable a t m => a -> m t -> m t
inferM a t = t <* (infer_ a =<< t)
