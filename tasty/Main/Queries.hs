module Main.Queries where

import BasePrelude
import XMLQuery
import qualified Data.Text as Text


query1 :: Element (Text.Text, Text.Text, [Text.Text])
query1 =
  elementNameIs "response" *>
  elementNodes (nodesEventualNode (nodeElement element))
  where
    element =
      elementNameIs "result" *>
      elementNodes (nodesEventualNode (nodeElement element))
      where
        element =
          elementNameIs "doc" *>
          ((,,) <$> author <*> isbn <*> mainCatalogueArea)
          where
            author =
              elementNodes (nodesEventualNode (nodeElement element))
              where
                element =
                  elementAttr attr *>
                  elementNodes nodes
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "author"
                    nodes =
                      nodesEventualNode (nodeElement (elementNodes (nodesEventualNode (nodeText textValue))))
            isbn =
              elementNodes (nodesEventualNode (nodeElement element))
              where
                element =
                  elementAttr attr *>
                  elementNodes (nodesEventualNode (nodeText textValue))
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "isbn"
            mainCatalogueArea =
              elementNodes (nodesEventualNode (nodeElement element))
              where
                element =
                  elementAttr attr *>
                  elementNodes (many (nodesEventualNode (nodeElement (elementNodes (nodesEventualNode (nodeText textValue))))))
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "main_catalog_area"



