module XML.Query.XMLTypes where

import XML.Query.XMLTypes.Prelude
import Data.XML.Types
import qualified XML.Query as Query
import qualified XML.Query.XMLTypes.Interpreter as Interpreter
import qualified Success.Pure as Success


nodes :: Query.Nodes a -> [Node] -> Either (Maybe Text) a
nodes query input =
  Success.asEither (Interpreter.nodes query input)
