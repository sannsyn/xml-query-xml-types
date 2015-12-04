module Main.Queries where

import BasePrelude
import XML.Query
import qualified Data.Text as Text


query1 :: Tag (Text.Text, Text.Text, [Text.Text])
query1 =
  tagNameIs "response" *>
  tagNodes (nodesNodeAnywhere (nodeTag tag))
  where
    tag =
      tagNameIs "result" *>
      tagNodes (nodesNodeAnywhere (nodeTag tag))
      where
        tag =
          tagNameIs "doc" *>
          ((,,) <$> author <*> isbn <*> mainCatalogueArea)
          where
            author =
              tagNodes (nodesNodeAnywhere (nodeTag tag))
              where
                tag =
                  tagAttr attr *>
                  tagNodes nodes
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "author"
                    nodes =
                      nodesNodeAnywhere (nodeTag (tagNodes (nodesNodeAnywhere (nodeText textValue))))
            isbn =
              tagNodes (nodesNodeAnywhere (nodeTag tag))
              where
                tag =
                  tagAttr attr *>
                  tagNodes (nodesNodeAnywhere (nodeText textValue))
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "isbn"
            mainCatalogueArea =
              tagNodes (nodesNodeAnywhere (nodeTag tag))
              where
                tag =
                  tagAttr attr *>
                  tagNodes (many (nodesNodeAnywhere (nodeTag (tagNodes (nodesNodeAnywhere (nodeText textValue))))))
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "main_catalog_area"



