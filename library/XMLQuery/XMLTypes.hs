module XMLQuery.XMLTypes where

import XMLQuery.XMLTypes.Prelude
import Data.XML.Types
import qualified XMLQuery as Query
import qualified XMLQuery.XMLTypes.Interpreter as Interpreter
import qualified Success.Pure as Success


element :: Query.Element a -> Element -> Either (Maybe Text) a
element query input =
  Success.asEither (Interpreter.element query input)

nodes :: Query.Nodes a -> [Node] -> Either (Maybe Text) a
nodes query input =
  Success.asEither (Interpreter.nodes query input)
