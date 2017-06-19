module Luna.Manager.Repository where

import Prologue

import Luna.Manager.Version
import Luna.Manager.System
import Luna.Manager.System.Path
import Luna.Manager.Config.Aeson

import Data.Map                      (Map)
import Data.Aeson                    (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON


------------------------
-- === Repository === --
------------------------

-- === Definition === --

data    Repo        = Repo        { _apps   :: Map Text Package, _libs    :: Map Text Package } deriving (Show, Generic, Eq)
newtype Package     = Package     { _pkgMap :: PkgMap                                         } deriving (Show, Generic, Eq)
data    PackageDesc = PackageDesc { _deps   :: [PackageDep]    , _path    :: URIPath          } deriving (Show, Generic, Eq)
data    PackageDep  = PackageDep  { _name   :: Text            , _version :: Version          } deriving (Show, Generic, Eq)
type PkgMap = Map Version (Map SysDesc PackageDesc)


-- === Instances === --

-- Monoids
instance Mempty    Repo where mempty = Repo mempty mempty
instance Semigroup Repo where Repo a l <> Repo a' l' = Repo (a <> a') (l <> l')

-- JSON
instance ToJSON   Repo        where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   Package     where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   PackageDesc where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   PackageDep  where toEncoding = JSON.toEncoding . encodeShow; toJSON = JSON.toJSON . encodeShow
instance FromJSON Repo        where parseJSON  = lensJSONParse
instance FromJSON Package     where parseJSON  = lensJSONParse
instance FromJSON PackageDesc where parseJSON  = lensJSONParse
instance FromJSON PackageDep  where parseJSON  = lensJSONParse

-- Lenses
makeLenses ''Repo
makeLenses ''Package
makeLenses ''PackageDesc
makeLenses ''PackageDep

-- Show
instance EncodeShow PackageDep where
    encodeShow (PackageDep n v) = n <> "-" <> encodeShow v
