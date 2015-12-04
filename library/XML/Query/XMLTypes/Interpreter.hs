module XML.Query.XMLTypes.Interpreter where

import XML.Query.XMLTypes.Prelude
import Data.XML.Types
import qualified XML.Query.AST as AST
import qualified XML.Query.XMLTypes.Renderer as Renderer
import qualified Success.Pure as Success


type Result =
  Success Text

nodes :: Alt AST.Nodes a -> [Node] -> Result a
nodes (Alt alternatives) input =
  runAlternatives alternatives input
  where
    runAlternatives alternatives input =
      asum (map (\alternative -> runAlternative alternative input) alternatives)
    runAlternative alternative input =
      case alternative of
        Ap fa altfab ->
          case fa of
            AST.NodesNode query ->
              case input of
                head : tail ->
                  nodes altfab tail <*>
                  node query head
                _ ->
                  Success.failure "No more nodes"
        Pure a ->
          Success.success a

node :: Alt AST.Node a -> Node -> Result a
node alt node =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Node a -> Result a
    interpreter =
      \case
        AST.NodeElement alt ->
          case node of
            NodeElement input ->
              element alt input
            _ ->
              Success.failure "Not an element node"
        AST.NodeText ast ->
          case node of
            NodeContent content ->
              text ast (Renderer.run Renderer.content content)
            _ ->
              Success.failure "Not a content node"

element :: Alt AST.Element a -> Element -> Result a
element alt input =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Element a -> Result a
    interpreter =
      \case
        AST.ElementNameText textAST ->
          text textAST (Renderer.run Renderer.name (elementName input))
        AST.ElementAttr attrAST ->
          asum (map (attr attrAST) (elementAttributes input))
        AST.ElementNodes ast ->
          nodes ast (elementNodes input)

attr :: Alt AST.Attr a -> (Name, [Content]) -> Result a
attr alt input =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Attr a -> Result a
    interpreter =
      \case
        AST.AttrNameText ast ->
          text ast (Renderer.run Renderer.name (fst input))
        AST.AttrValueText ast ->
          text ast (Renderer.run Renderer.contents (snd input))

text :: Alt AST.Text a -> Text -> Result a
text alt input =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Text a -> Result a
    interpreter =
      \case
        AST.Text fn ->
          either Success.failure Success.success (fn input)
