module Graphics.API where

-- import Graphics.API.Component      as X
-- import Graphics.API.Graphics       as X
-- import Graphics.API.Layer          as X
-- import Graphics.API.Material       as X
-- import Graphics.API.Shader         as X
-- import Graphics.API.Shape          as X
-- import Graphics.API.Transformation as X

import           Control.Lens
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Binary  (Binary)
import           Data.Default
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           GHC.Generics (Generic)



-- === Common === --

-- data One
-- data Two

data Point2 = Point2 Double Double
              deriving (Show, Eq, Generic)

instance Binary   Point2
instance ToJSON   Point2
instance FromJSON Point2

data Point3 = Point3 Double Double Double
              deriving (Show, Eq, Generic)


instance Binary   Point3
instance ToJSON   Point3
instance FromJSON Point3

-- === Attributes === --

-- data AttributeType = Point | Vertex | Face | Detail


type AttributeType = String

-- data Attributes = Attributes (Hmap (AttributeType, Key))
-- data Attributes = Attributes { _attr :: Hmap (AttrKey T A) -> A } deriving (Show, Eq, Generic)

-- data AttrKey a b = AttrKey

-- lookup (AttrKey::AttrKey Attr.Point Attr.Color)


data Attributes = Attributes (Map.Map AttributeType Double)
                  deriving (Show, Eq, Generic)

instance Binary   Attributes
instance ToJSON   Attributes
instance FromJSON Attributes

-- === Figure === --

data Figure = Square    { _s :: Double }
            | Rectangle { _w :: Double
                        , _h :: Double }
            | Circle    { _d :: Double }
            deriving (Show, Eq, Generic)

instance Binary   Figure
instance ToJSON   Figure
instance FromJSON Figure

-- === Primitive === --

data Primitive = Primitive Figure Point2 Attributes
                 deriving (Show, Eq, Generic)


instance Binary   Primitive
instance ToJSON   Primitive
instance FromJSON Primitive

-- === Shape === --

data Shape = Single    Primitive
           | Merge     Shape Shape
           | Subtract  Shape Shape
           | Intersect Shape Shape
           deriving (Show, Eq, Generic)

instance Binary   Shape
instance ToJSON   Shape
instance FromJSON Shape

-- === Surface === --

data Surface = ShapeSurface Shape
             | PolygonSurface
             | NumbsSurface
             deriving (Show, Eq, Generic)

instance Binary   Surface
instance ToJSON   Surface
instance FromJSON Surface


-- === Transformation === --
-- TODO: matrix + methods

data Trans = Trans { _scaleX :: Double
                   , _scaleY :: Double
                   , _dx     :: Double
                   , _dy     :: Double
                   , _angle  :: Double
                   , _refl   :: Bool
                   } deriving (Show, Eq, Generic)

instance Default Trans where
    def = Trans 1.0 1.0 0.0 0.0 0.0 False

makeLenses ''Trans

instance Binary   Trans
instance ToJSON   Trans
instance FromJSON Trans


-- === Material === --

data Material = SolidColor { _r :: Double
                           , _g :: Double
                           , _b :: Double
                           , _a :: Double
                           } deriving (Show, Eq, Generic)


instance Binary   Material
instance ToJSON   Material
instance FromJSON Material

-- === Geometry === --

data Geo = Geo [Surface] Trans Material
         | Group [Geo]   Trans Material
         deriving (Show, Eq, Generic)

instance Binary   Geo
instance ToJSON   Geo
instance FromJSON Geo
