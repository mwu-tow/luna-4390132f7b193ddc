{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Show_old where

import Prelude
import Data.Typeable
import Data.Monoid
import GHC.TypeLits
import Text.Show.Pretty (ppValue, parseValue)
import Text.PrettyPrint (Doc, text)
import GHC.Exts         (Constraint)

--showType :: Typeable a => a -> String
--showType (a :: a) = showProxyType (Proxy :: Proxy a)

--showProxyType :: Typeable (Proxy a) => Proxy a -> String
--showProxyType = clear . drop 6 . show . typeOf where
--    clear = \case ('(':s)     -> init  s
--                  ('*':' ':s) -> clear s -- PolyKinded Proxies are shown as "Proxy *"
--                  s           -> s


class TypeShow a where
    showType :: Proxy a -> String
    default showType :: Typeable a => Proxy a -> String
    showType = show . typeRep ; {-# INLINE showType #-}

showType' :: forall t. TypeShow t => String
showType' = showType (Proxy :: Proxy t) ; {-# INLINE showType' #-}

printType :: TypeShow a => Proxy a -> IO ()
printType = putStrLn . showType

ppPrintType :: TypeShow a => Proxy a -> IO ()
ppPrintType = putStrLn . ppShowType

ppShowType :: TypeShow a => Proxy a -> String
ppShowType = show . ppTypeDoc

ppTypeDoc :: TypeShow a => Proxy a -> Doc
ppTypeDoc a = case parseValue txt of
            Just v  -> ppValue v
            Nothing -> text txt
  where txt = showType a



-- === Basic Instances === --

instance KnownNat n               => TypeShow (n :: Nat) where showType _ = show $ natVal (Proxy :: Proxy n)
instance ListElemsShow a          => TypeShow (a :: [k]) where showType _ = "[" <> showListElems (Proxy :: Proxy a) <> "]"
instance (TypeShow a, TypeShow b) => TypeShow '(a,b)     where showType _ = "(" <> showType (Proxy :: Proxy a) <> ", " <> showType (Proxy :: Proxy b) <> ")"


-- === List Helpers === --

class                                                           ListElemsShow a         where showListElems :: Proxy a -> String
instance {-# OVERLAPPABLE #-}                                   ListElemsShow '[]       where showListElems _ = ""
instance {-# OVERLAPPABLE #-}  TypeShow a                    => ListElemsShow '[a]      where showListElems _ = showType (Proxy :: Proxy a)
instance {-# OVERLAPPABLE #-} (TypeShow a, ListElemsShow as) => ListElemsShow (a ': as) where showListElems _ = showType (Proxy :: Proxy a) <> ", " <> showListElems (Proxy :: Proxy as)
