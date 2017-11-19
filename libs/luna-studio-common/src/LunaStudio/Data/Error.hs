module LunaStudio.Data.Error where

import           Control.DeepSeq  (NFData)
import           Data.Aeson.Types (ToJSON)
import           Data.Binary      (Binary)
import           Prologue


data ErrorType = CompileError | RuntimeError deriving (Eq, Generic, Show)

data Error = Error { _errorType    :: ErrorType
                   , _errorContent :: Text
                   } deriving (Eq, Generic, Show)

makeLenses ''Error

instance Binary ErrorType
instance NFData ErrorType
instance ToJSON ErrorType
instance Binary Error
instance NFData Error
instance ToJSON Error
