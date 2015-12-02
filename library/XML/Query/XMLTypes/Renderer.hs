module XML.Query.XMLTypes.Renderer where

import XML.Query.XMLTypes.Prelude
import Data.XML.Types
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy as LazyText
import qualified HTMLEntities.Decoder


type Renderer a =
  a -> Builder.Builder

run :: Renderer a -> a -> Text
run renderer input =
  LazyText.toStrict $
  Builder.toLazyText $
  renderer input

name :: Renderer Name
name =
  \(Name local ns prefix) ->
    foldMap (\x -> Builder.fromText x <> Builder.singleton ':') prefix <>
    Builder.fromText local

contents :: Renderer [Content]
contents =
  foldMap content
  
content :: Renderer Content
content =
  \case
    ContentText x ->
      Builder.fromText x
    ContentEntity x -> 
      either (error "XML.Query.XMLTypes.Renderer.content: corrupt ContentEntity") Builder.fromText $
      HTMLEntities.Decoder.htmlEntity x