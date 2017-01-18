module Graphics.API where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Data.Matrix     (Matrix)
import qualified Data.Text       as Text
import           GHC.Generics    (Generic)
import           Prelude


------------------
-- === Math === --
------------------
type Trans = Matrix Double

data Point = Point { _x :: Double
                   , _y :: Double
                   } deriving (Eq, Generic, NFData, Show)

makeLenses ''Point

data Curve = Curve deriving (Eq, Generic, NFData, Show) -- TODO select Curve implementation

-- === Boolean === --

data Boolean a = Union        a a
               | Difference   a a
               | Intersection a a
               deriving (Eq, Generic, NFData, Show)


------------------------
-- === ColorTypes === --
------------------------

type ColorVal = Double


-- === Translucency === --

class HasComponentA t where a :: Lens' t ColorVal


-- === RGBAA === --

data RGBA = RGBA { _rgba_r :: ColorVal
                 , _rgba_g :: ColorVal
                 , _rgba_b :: ColorVal
                 , _rgba_a :: ColorVal
                 } deriving (Eq, Generic, NFData, Show)
makeLenses ''RGBA

class HasComponentR t where r :: Lens' t ColorVal
class HasComponentG t where g :: Lens' t ColorVal
class HasComponentB t where b :: Lens' t ColorVal

instance HasComponentR RGBA where r = rgba_r
instance HasComponentG RGBA where g = rgba_g
instance HasComponentB RGBA where b = rgba_b
instance HasComponentA RGBA where a = rgba_a

class HasRGBA t where rgba :: Lens' t RGBA


-- === RGBAA === --

data HSVA = HSVA { _hsv_h :: ColorVal
                 , _hsv_s :: ColorVal
                 , _hsv_v :: ColorVal
                 , _hsv_a :: ColorVal
                 } deriving (Eq, Generic, NFData, Show)
makeLenses ''HSVA

class HasComponentH t where h :: Lens' t ColorVal
class HasComponentS t where s :: Lens' t ColorVal
class HasComponentV t where v :: Lens' t ColorVal

instance HasComponentH HSVA where h = hsv_h
instance HasComponentS HSVA where s = hsv_s
instance HasComponentV HSVA where v = hsv_v
instance HasComponentA HSVA where a = hsv_a

instance HasRGBA HSVA where rgba = error "todo"



-------------------
-- === Color === --
-------------------

data Color = Solid RGBA
           {- | Gradient -} -- kiedys
           deriving (Eq, Generic, NFData, Show)




-- === Material === --

data Layer = Diffuse Color
           | Shadow  Size Color
           | Border  Size Color
           deriving (Eq, Generic, NFData, Show)

data Material = Material [Layer] deriving (Eq, Generic, NFData, Show)


-- === Text === --

data AlignmentH = Left | Center | Right deriving (Eq, Generic, NFData, Show)
data Font    = Font    { _family :: Text.Text, _size :: Int } deriving (Eq, Generic, NFData, Show)
data TextDef = TextDef { _txt :: Text.Text, _font :: Font, _aligment :: AlignmentH } deriving (Eq, Generic, NFData, Show)


-- === Geometry === --

type Width   = Double
type Height  = Double
type Size    = Double
type Radius  = Double
type RadiusX = Double
type RadiusY = Double

data Shape = Square    Size
           | Rectangle Width Height
           | Circle    Radius
           | Ellipse   RadiusX RadiusY
           | Line      Width Point
           | PolyLine  [Point]
           | Polygon   [Point]
           | Path      [Curve]
           | Text      TextDef
           deriving (Eq, Generic, NFData, Show)

data Surface = Simple   Shape
             | Group    [Geometry]
             | Compound (Boolean Geometry)
             deriving (Eq, Generic, NFData, Show)

data Geometry = Geometry Material Trans Surface deriving (Eq, Generic, NFData, Show)
