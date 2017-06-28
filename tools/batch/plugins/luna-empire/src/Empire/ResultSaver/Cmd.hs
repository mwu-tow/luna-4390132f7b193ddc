module Empire.ResultSaver.Cmd where

import Prologue

data Cmd = Save          { projectId :: String
                         , outFile   :: Maybe String
                         }
         | ImportAndSave { inFile   :: String
                         , outFile' :: String
                         }
         | Version
         deriving Show

