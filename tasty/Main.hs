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
import qualified XMLQuery as XMLQuery
import qualified XMLQuery.XMLTypes as XMLQueryXMLTypes
import qualified Text.XML as XMLConduit
import qualified Data.XML.Types as XMLTypes


main =
  defaultMain tree

tree =
  testGroup "All tests"
  [
    testCase "sample 1" $
      queryFile Queries.query1 "samples/1.xml" >>=
      \x -> assertEqual (show x) (Right ("Ehrlin, Carl-Johan Forssén", "9788202505929", ["Bøker", "Barnas ARK"])) x
  ]

queryFile :: XMLQuery.Element a -> String -> IO (Either (Maybe Text) a)
queryFile query filename =
  XMLConduit.readFile def filename >>=
  pure . XMLQueryXMLTypes.element query . XMLTypes.documentRoot . XMLConduit.toXMLDocument

