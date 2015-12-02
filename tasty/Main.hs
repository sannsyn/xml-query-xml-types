module Main where

import BasePrelude hiding (assert)
import Data.Default.Class
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.Tasty.QuickCheck
import Data.Text (Text)
import qualified Test.QuickCheck as QuickCheck
import qualified Main.Queries as Queries
import qualified XML.Query as XMLQuery
import qualified XML.Query.XMLTypes as XMLQueryXMLTypes
import qualified Text.XML as XMLConduit
import qualified Data.XML.Types as XMLTypes


main =
  defaultMain tree

tree =
  testGroup "All tests"
  [
    testCase "sample 1" $
      queryFile Queries.query1 "samples/1.xml" >>=
      \x -> assertEqual (show x) (Right ("Ehrlin, Carl-Johan Forssén", "9788202505929")) x
  ]

queryFile :: XMLQuery.Nodes a -> String -> IO (Either (Maybe Text) a)
queryFile query filename =
  XMLConduit.readFile def filename >>=
  pure . XMLQueryXMLTypes.nodes query . XMLTypes.elementNodes . XMLTypes.documentRoot . XMLConduit.toXMLDocument

