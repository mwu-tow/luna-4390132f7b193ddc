module LunaStudio.API.Graph.SearchNodes where

import Prologue

import qualified LunaStudio.API.Graph.Request as G
import qualified LunaStudio.API.Request       as R
import qualified LunaStudio.API.Response      as Response
import qualified LunaStudio.API.Topic         as T

import Data.Aeson.Types              (ToJSON)
import Data.Binary                   (Binary)
import Data.Map                      (Map)
import Data.Set                      (Set)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.Searcher.Node (LibraryHints, LibraryName)


data Request = Request
    { _location         :: GraphLocation
    , _missingLibraries :: Set LibraryName
    } deriving (Eq, Generic, Show)

data Result = Result
    { _searcherHints :: Map LibraryName LibraryHints
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.nodesearch"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response
