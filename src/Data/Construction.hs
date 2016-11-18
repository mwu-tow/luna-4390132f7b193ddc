{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Construction where

import Prelude hiding (break)
import Control.Lens
import qualified Data.RTuple as List
import           Data.RTuple (List)
import           Type.Applicative
import           Data.Constraints
import           Data.Text.Lazy hiding (break)


-- === Definitions === --

type family Deconstructed a
type family Args          a -- Depreciated

class            Maker           a where make        :: Deconstructed a -> a
class            Breaker         a where break       :: a -> Deconstructed a
class Monad m => Constructor a m t where cons        :: a -> m t

class Monad m => Constructor'  m a where construct'  :: Deconstructed a -> m a   ; default construct' :: Creator m a => Deconstructed a -> m a
                                                                                 ; construct' _ = create ; {-# INLINE construct' #-}
class Monad m => Deconstructor m a where deconstruct :: a -> m (Deconstructed a) ; default deconstruct :: Breaker a => a -> m (Deconstructed a)
                                                                                 ; deconstruct = return . break ; {-# INLINE deconstruct #-}
class Monad m => Creator       m a where create      :: m a                      ; default create :: Maker a => Deconstructed a -> m a
                                                                                 ; create = return . make ; {-# INLINE create #-}
class Monad m => Generator     m a where new         :: m a                      ; default new :: Maker a => Deconstructed a -> m a
                                                                                 ; new = return . make ; {-# INLINE new #-}
class Monad m => Destructor    m a where destruct    :: a -> m ()                ; default destruct :: Deconstructor m a => a -> m ()
                                                                                 ; destruct = deconstruct_ ; {-# INLINE destruct #-}

-- Producers & Consumers

class Monad m => Producer m a where produce :: m a
class Monad m => Consumer m a where consume :: a -> m ()

-- Registration

class Monad m => Register   m a where register   :: a -> m ()
class Monad m => Unregister m a where unregister :: a -> m ()




-- === Products === --

type family Fields        a :: [*]

class Product  a b where fields  :: Iso  a b (List (Fields a)) (List (Fields b))
class Product' a   where fields' :: Iso' a   (List (Fields a))

product :: (Product s s, List.Curry' f, List.Uncurried' f ~ (List (Fields s) -> s)) => f
product = List.curry' $ view (from fields) ; {-# INLINE product #-}

product' :: (Product' s, List.Curry' f, List.Uncurried' f ~ (List (Fields s) -> s)) => f
product' = List.curry' $ view (from fields') ; {-# INLINE product' #-}


deconstruct_ :: Deconstructor m a => a -> m ()
deconstruct_ a = () <$ deconstruct a ; {-# INLINE deconstruct_ #-}


type ConsFields cons a = Constraints (cons <$> Fields a)
type ShowFields      a = ConsFields Show a



-- === FieldNames === --

class HasFieldNames a where
    fieldNames :: a -> [Text]

type family FieldsType a
class HasFields a where
    fieldList :: a -> [FieldsType a]
