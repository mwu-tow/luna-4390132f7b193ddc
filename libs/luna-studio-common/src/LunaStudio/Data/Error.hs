module LunaStudio.Data.Error where

import           Data.Aeson.Types (ToJSON)
import           Control.DeepSeq  (NFData)
import           Data.Binary      (Binary)
import           Prologue


data ErrorType = CompileError | RuntimeError deriving (Eq, Generic, Show)

data Error = Error ErrorType Text deriving (Eq, Generic, Show)

instance Binary ErrorType
instance NFData ErrorType
instance ToJSON ErrorType
instance Binary Error
instance NFData Error
instance ToJSON Error
