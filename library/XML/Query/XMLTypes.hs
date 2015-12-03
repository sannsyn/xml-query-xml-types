module XML.Query.XMLTypes where

import XML.Query.XMLTypes.Prelude
import Data.XML.Types
import qualified XML.Query as Query
import qualified XML.Query.XMLTypes.Interpreter as Interpreter
import qualified Success.Pure as Success


tag :: Query.Tag a -> Element -> Either (Maybe Text) a
tag query input =
  Success.asEither (Interpreter.tag query input)

