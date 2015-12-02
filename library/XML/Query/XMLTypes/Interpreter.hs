module XML.Query.XMLTypes.Interpreter where

import XML.Query.XMLTypes.Prelude
import Data.XML.Types
import qualified XML.Query.AST as AST
import qualified XML.Query.XMLTypes.Renderer as Renderer
import qualified Success.Pure as Success


type Result =
  Success Text

nodes :: Alt AST.Nodes a -> [Node] -> Result a
nodes alt nodes =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Nodes a -> Result a
    interpreter =
      \case
        AST.NodesNode alt ->
          asum (map (node alt) nodes)

node :: Alt AST.Node a -> Node -> Result a
node alt node =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Node a -> Result a
    interpreter =
      \case
        AST.NodeTag alt ->
          case node of
            NodeElement element ->
              tag alt element
            _ ->
              Success.failure "Not an element node"
        AST.NodeText ast ->
          case node of
            NodeContent content ->
              text ast (Renderer.run Renderer.content content)
            _ ->
              Success.failure "Not a content node"

tag :: Alt AST.Tag a -> Element -> Result a
tag alt element =
  runAlt interpreter alt
  where
    interpreter :: forall a. AST.Tag a -> Result a
    interpreter =
      \case
        AST.TagNameText textAST ->
          text textAST (Renderer.run Renderer.name (elementName element))
        AST.TagAttr attrAST ->
          asum (map (attr attrAST) (elementAttributes element))
        AST.TagNodes ast ->
          nodes ast (elementNodes element)

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
