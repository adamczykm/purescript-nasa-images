module API.NasaImages.Validation where

import Prelude

import API.NasaImages (Metadata(..), SearchItem(..), SearchResult(..))
import Data.Record (insert)
import Data.Record.Fold (collect)
import Data.Symbol (SProxy(..))
import Validators.Json (JsValidation, arrayOf, elem, field, int, optionalField, string)

metadata :: forall m. Monad m => JsValidation m Metadata
metadata = Metadata <$> (collect { "totalHits": field "total_hits" int })

searchItem :: forall m. Monad m => JsValidation m SearchItem
searchItem = SearchItem <$> (
  insert (SProxy :: SProxy "collection") <$>
    (field "href" string) <*>
    (field "data" $ elem 0 $ collect
      { title: field "title" string
      , description: field "description" string
      , keywords: optionalField "keywords" $ arrayOf string
      , nasaId: field "nasa_id" string
      }))

searchResult :: forall m. Monad m => JsValidation m SearchResult
searchResult = SearchResult <$> (collect
  { href: field "href" string
  , items: field "items" $ arrayOf searchItem
  , metadata: field "metadata" $ metadata
  })
