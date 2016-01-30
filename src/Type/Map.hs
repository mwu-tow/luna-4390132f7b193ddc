{-# LANGUAGE PolyKinds #-}

module Type.Map where

data Map k v = Map [(k,v)]

type family MapLookup (k :: kk) (m :: Map kk kv) :: kv where
    MapLookup k ('Map ( '(k, v) ': ms )) = v
    MapLookup k ('Map ( '(l, v) ': ms )) = MapLookup k ('Map ms)