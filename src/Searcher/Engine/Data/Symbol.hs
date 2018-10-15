module Searcher.Engine.Data.Symbol
    ( module Searcher.Engine.Data.Symbol
    , module X
    ) where

import Searcher.Engine.Data                as X
import Searcher.Engine.Data.Score          as X (Weight)
import Searcher.Engine.Data.Symbol.Library as X (Library (Library))
import Searcher.Engine.Prelude             as X (Documentation, Name)

import Searcher.Engine.Prelude

import qualified Searcher.Engine.Data.Symbol.Library as Library

import Searcher.Engine.Data.Score (Score (Score))


type ClassName = Name

data Kind
    = Function
    | Constructor ClassName
    | Method      ClassName
    deriving (Eq, Show)

makePrisms ''Kind


data Symbol = Symbol
    { _symbolName          :: Name
    , _library             :: Library
    , _kind                :: Kind
    , _symbolDocumentation :: Documentation
    , _weight              :: Weight
    } deriving (Eq, Show)

makeLenses ''Symbol

instance SearcherData Symbol where
    name              = symbolName
    rawDocumentation  = symbolDocumentation
    prefix            = library . Library.name
    hintTextSeparator = to $ const " . "
    score =
        let importScore s = if s ^. library . Library.imported
                then Library.importedBonus
                else Library.notImportedPenalty
            getter s = Score (importScore s) def (s ^. weight)
        in to getter

parentClass :: Getter Symbol (Maybe ClassName)
parentClass = to $ classGetter . kindGetter where
    kindGetter s = s ^. kind
    classGetter Function                = Nothing
    classGetter (Constructor className) = Just className
    classGetter (Method      className) = Just className
