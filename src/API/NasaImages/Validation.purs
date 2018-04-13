module API.NasaImages.Validation where

import Prelude

import API.NasaImages (Metadata(..), SearchItem(..), SearchResult(..))
import Data.Record.Fold (collect)
import Validators.Json (JsValidation, arrayOf, elem, field, int, optionalField, string)

metadata :: forall m. Monad m => JsValidation m Metadata
metadata = Metadata <$> (collect { "totalHits": field "total_hits" int })

searchItem :: forall m. Monad m => JsValidation m SearchItem
searchItem = SearchItem <$> (collect
  { title: field "title" string
  , description: field "description" string
  , keywords: optionalField "keywords" $ arrayOf string
  , nasaId: field "nasa_id" string
  })

searchResult :: forall m. Monad m => JsValidation m SearchResult
searchResult = SearchResult <$> (collect
  { href: field "href" string
  , items: field "items" $ arrayOf (field "data" (elem 0 searchItem))
  , metadata: field "metadata" $ metadata
  })
