module LunaStudio.API.Graph.GetProgram where

import           Data.Binary                          (Binary (..))
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import qualified LunaStudio.API.Graph.Request         as G
import qualified LunaStudio.API.Request               as R
import qualified LunaStudio.API.Response              as Response
import qualified LunaStudio.API.Topic                 as T
import           LunaStudio.Data.Breadcrumb           (Breadcrumb, BreadcrumbItem, Named)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.Graph                (Graph)
import           LunaStudio.Data.GraphLocation        (GraphLocation)
import           LunaStudio.Data.NodeValue            (Visualizer)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           Prologue                             hiding (TypeRep)


data Request = Request { _location  :: GraphLocation
                       , _newModule :: Bool
                       } deriving (Eq, Generic, NFData, Show)

type Error = String

data Result  = Result  { _graph                   :: Either Error Graph
                       , _code                    :: Text
                       , _breadcrumb              :: Breadcrumb (Named BreadcrumbItem)
                       , _newModuleVisPreferences :: HashMap TypeRep Visualizer
                       , _visualizerPreferences   :: HashMap TypeRep Visualizer
                       , _camera                  :: Maybe CameraTransformation
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Result

--FIXME[MM, LJK, PM]: We should allow sending HashMap here without convert to list
instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HashMap k v) where
    put = put . HashMap.toList
    get = HashMap.fromList . get


instance Binary Request
instance Binary Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.program"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
