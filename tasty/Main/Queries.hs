module Main.Queries where

import BasePrelude
import XML.Query
import qualified Data.Text as Text


query1 :: Tag (Text.Text, Text.Text, [Text.Text])
query1 =
  tagNameIs "response" *>
  tagNodes (nodesNode (nodeTag tag))
  where
    tag =
      tagNameIs "result" *>
      tagNodes (nodesNode (nodeTag tag))
      where
        tag =
          tagNameIs "doc" *>
          tagNodes nodes
          where
            nodes =
              (,,) <$> nodesNode (nodeTag author) <*> nodesNode (nodeTag isbn) <*> nodesNode (nodeTag mainCatalogueArea)
              where
                author =
                  tagAttr attr *>
                  tagNodes nodes
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "author"
                    nodes =
                      nodesNode (nodeTag (tagNodes (nodesNode (nodeText textValue))))
                isbn =
                  tagAttr attr *>
                  tagNodes (nodesNode (nodeText textValue))
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "isbn"
                mainCatalogueArea =
                  tagAttr attr *>
                  tagNodes (many (nodesNode (nodeTag (tagNodes (nodesNode (nodeText textValue))))))
                  where
                    attr =
                      attrNameIs "name" *>
                      attrValueIs "main_catalog_area"



