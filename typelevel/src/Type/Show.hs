{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications     #-}

module Type.Show where

import Prelude
import Data.Typeable
import Data.Monoid
import GHC.TypeLits
import Control.Monad.IO.Class
import Text.Show.Pretty (ppValue, parseValue)
import Text.PrettyPrint (Doc, text)
import GHC.Exts         (Constraint)


----------------------
-- === TypeShow === --
----------------------

-- === Definition === --

class Typeable a => TypeShow a where
    showType :: String

instance {-# OVERLAPPABLE #-} Typeable a => TypeShow a where
    showType = show $ typeRep (Proxy :: Proxy a) ; {-# INLINE showType #-}


-- === Utils === --

printType, ppPrintType :: forall a m. (TypeShow a, MonadIO m) => m ()
printType   = liftIO . putStrLn $ showType   @a ; {-# INLINE printType   #-}
ppPrintType = liftIO . putStrLn $ ppShowType @a ; {-# INLINE ppPrintType #-}

ppShowType :: forall a. TypeShow a => String
ppTypeDoc  :: forall a. TypeShow a => Doc
ppShowType = show $ ppTypeDoc @a ; {-# INLINE ppShowType #-}
ppTypeDoc  = case parseValue txt of
            Just v  -> ppValue v
            Nothing -> text txt
  where txt = showType @a
{-# INLINE ppTypeDoc #-}


-- === Basic Instances === --

instance KnownNat n                                => TypeShow (n :: Nat) where showType = show $ natVal (Proxy :: Proxy n)
instance (ListElemsShow a, Typeable a)             => TypeShow (a :: [k]) where showType = "[" <> showListElems @a <> "]"
instance (TypeShow a, TypeShow b, Typeable '(a,b)) => TypeShow '(a,b)     where showType = "(" <> showType @a <> ", " <> showType @b <> ")"


-- === List Helpers === --

class                                                           ListElemsShow a         where showListElems :: String
instance {-# OVERLAPPABLE #-}                                   ListElemsShow '[]       where showListElems = ""
instance {-# OVERLAPPABLE #-}  TypeShow a                    => ListElemsShow '[a]      where showListElems = showType @a
instance {-# OVERLAPPABLE #-} (TypeShow a, ListElemsShow as) => ListElemsShow (a ': as) where showListElems = showType @a <> ", " <> showListElems @as
