{-# LANGUAGE PolyKinds #-}

module Type.Functor where


infixl 4 <$>

type f <$> as = FMap f as
type family FMap (f :: l -> k) (as :: [l]) :: [k] where
            FMap f             '[]         = '[]
            FMap f             (a ': as)   = f a ': FMap f as
