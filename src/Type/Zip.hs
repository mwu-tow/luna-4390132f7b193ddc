{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Zip where


type family Zip lst lst' where Zip lst lst' = ZipWith '(,) lst lst' -- Implemented as TF because #11375

type family ZipWith f lst lst' where
    ZipWith f '[]       lst       = '[]
    ZipWith f lst       '[]       = '[]
    ZipWith f (l ': ls) (n ': ns) = f l n ': ZipWith f ls ns