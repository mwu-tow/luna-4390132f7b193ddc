module Graphics.API where

import           Control.Lens
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Data.Convert
import           Data.Default
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           GHC.Generics     (Generic)



-- === Common === --


data Point = Point Double Double
             deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Point3 = Point3 Double Double Double
              deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)


-- === Material === --

-- TODO: expand possibilities
data Material = SolidColor { _r :: Double
                           , _g :: Double
                           , _b :: Double
                           , _a :: Double
                           } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)


-- === Transformation === --

-- TODO: matrix + methods
data Transformation = Transformation { _scaleX :: Double
                                     , _scaleY :: Double
                                     , _dx     :: Double
                                     , _dy     :: Double
                                     , _angle  :: Double
                                     , _refl   :: Bool
                                     } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

makeLenses ''Transformation


scale :: Transformation -> Double -> Double -> Transformation
scale (Transformation sx sy dx dy a r) sx' sy' = Transformation (sx * sx') (sy * sy') dx dy a r
{-# INLINE scale #-}

translate :: Transformation -> Double -> Double -> Transformation
translate (Transformation sx sy dx dy a r) dx' dy' = Transformation sx sy (dx + dx') (dy + dy') a r
{-# INLINE translate #-}

rotate :: Transformation -> Double -> Transformation
rotate (Transformation sx sy dx dy a r) a' = Transformation sx sy dx dy (a + a') r
{-# INLINE rotate #-}

reflect :: Transformation -> Transformation
reflect (Transformation sx sy dx dy a r) = Transformation sx sy dx dy a (not r)
{-# INLINE reflect #-}


-- === Attributes === --

-- data AttributeType = Point | Vertex | Face | Detail


type AttributeType = String

-- data Attributes = Attributes (Hmap (AttributeType, Key))
-- data Attributes = Attributes { _attr :: Hmap (AttrKey T A) -> A } deriving (Show, Eq, Generic)

-- data AttrKey a b = AttrKey

-- lookup (AttrKey::AttrKey Attr.Point Attr.Color)

data Attributes = Attributes (Map.Map AttributeType Double)
                  deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)


-- === Shader figures === --

-- TODO: Convertibles
data Figure = Square    { _s :: Double }
            | Rectangle { _w :: Double
                        , _h :: Double }
            | Circle    { _d :: Double }
            deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

-- TODO: remove position
data Primitive = Primitive { _figure     :: Figure
                           , _position   :: Point
                           , _attributes :: Attributes
                           } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

-- TODO: add transformation (using auxiliary data type)
-- TODO: consider lists of merge and intersect
data Shape = Shape     Primitive
           | Merge     Shape Shape
           | Subtract  Shape Shape
           | Intersect Shape Shape
           deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Surface = ShapeSurface Shape
             | PolygonSurface
             | NumbsSurface
             deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Geometry = Geometry { _content        :: GeoComponent
                         , _transformation :: Transformation
                         , _material       :: Maybe Material
                         } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data GeoComponent = GeoElem  [Surface]
                  | GeoGroup [Geometry]
                  deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)


-- === Texts === --

data TextAlignment = Left | Center | Right
                   deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Label = Label { _labelPosition :: Point
                   , _fontSize      :: Double
                   , _textAlign     :: TextAlignment
                   , _text          :: String
                   } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

-- === Multiple shaders layers === --

data Placement = Transformations { _transformations :: [Transformation] }
               | Translations    { _translations    :: [Point]          }
               deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Labels = Labels { _labelsList :: [Label]
                     } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Layer = Layer { _geometry  :: Geometry
                   , _placement :: Placement
                   , _labels    :: Labels
                   } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)

data Graphics = Graphics { _graphics :: [Layer]
                         } deriving (Show, Eq, Generic, Binary, ToJSON, FromJSON)


-------------------------------
-- === Default instances === --
-------------------------------

instance Default Point where
    def = Point def def

instance Default Point3 where
    def = Point3 def def def

instance Default Material where
    def = SolidColor 1.0 1.0 1.0 1.0

instance Default Transformation where
    def = Transformation 1.0 1.0 0.0 0.0 0.0 False

instance Default Attributes where
    def = Attributes def

instance Default Placement where
    def = Translations [def]

instance Default Labels where
    def = Labels def

-----------------------------------
-- === Convertible instances === --
-----------------------------------

figureToPrimitive :: Figure -> Primitive
figureToPrimitive figure = Primitive figure def def
{-# INLINE figureToPrimitive #-}

primitiveToShape :: Primitive -> Shape
primitiveToShape = Shape
{-# INLINE primitiveToShape #-}

shapeToSurface :: Shape -> Surface
shapeToSurface = ShapeSurface
{-# INLINE shapeToSurface #-}

surfaceToGeoComponent :: Surface -> GeoComponent
surfaceToGeoComponent = GeoElem . pure
{-# INLINE surfaceToGeoComponent #-}

geoComponentToGeometry :: GeoComponent -> Geometry
geoComponentToGeometry geoComponent = Geometry geoComponent def (Just def)
{-# INLINE geoComponentToGeometry #-}

geometryToLayer :: Geometry -> Layer
geometryToLayer geometry = Layer geometry def def
{-# INLINE geometryToLayer #-}

layerToGraphics :: Layer -> Graphics
layerToGraphics = Graphics . pure
{-# INLINE layerToGraphics #-}

instance Convertible Layer        Graphics     where convert =           layerToGraphics
instance Convertible Geometry     Graphics     where convert = convert . geometryToLayer
instance Convertible GeoComponent Graphics     where convert = convert . geoComponentToGeometry
instance Convertible Surface      Graphics     where convert = convert . surfaceToGeoComponent
instance Convertible Shape        Graphics     where convert = convert . shapeToSurface
instance Convertible Primitive    Graphics     where convert = convert . primitiveToShape
instance Convertible Figure       Graphics     where convert = convert . figureToPrimitive

instance Convertible Geometry     Layer        where convert =           geometryToLayer
instance Convertible GeoComponent Layer        where convert = convert . geoComponentToGeometry
instance Convertible Surface      Layer        where convert = convert . surfaceToGeoComponent
instance Convertible Shape        Layer        where convert = convert . shapeToSurface
instance Convertible Primitive    Layer        where convert = convert . primitiveToShape
instance Convertible Figure       Layer        where convert = convert . figureToPrimitive

instance Convertible GeoComponent Geometry     where convert =           geoComponentToGeometry
instance Convertible Surface      Geometry     where convert = convert . surfaceToGeoComponent
instance Convertible Shape        Geometry     where convert = convert . shapeToSurface
instance Convertible Primitive    Geometry     where convert = convert . primitiveToShape
instance Convertible Figure       Geometry     where convert = convert . figureToPrimitive

instance Convertible Surface      GeoComponent where convert =           surfaceToGeoComponent
instance Convertible Shape        GeoComponent where convert = convert . shapeToSurface
instance Convertible Primitive    GeoComponent where convert = convert . primitiveToShape
instance Convertible Figure       GeoComponent where convert = convert . figureToPrimitive

instance Convertible Shape        Surface      where convert =           shapeToSurface
instance Convertible Primitive    Surface      where convert = convert . primitiveToShape
instance Convertible Figure       Surface      where convert = convert . figureToPrimitive

instance Convertible Primitive    Shape        where convert =           primitiveToShape
instance Convertible Figure       Shape        where convert = convert . figureToPrimitive

instance Convertible Figure       Primitive    where convert =           figureToPrimitive
