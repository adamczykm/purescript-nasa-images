module API.NasaImages.Validation where

import Prelude

import API.NasaImages (Metadata(..), SearchItem(..), SearchResult(..))
import Control.Monad.Error.Class (throwError)
import Data.Array (head)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readInt, readString)
import Data.Foreign.Index (readProp)
import Data.Traversable (traverse)

readMetadata :: Foreign -> F Metadata
readMetadata f = do
  th <- readProp "total_hits" f >>= readInt
  pure $ Metadata { totalHits: th }

readSearchItem :: Foreign -> F SearchItem
readSearchItem f = do
  title <- readProp "title" f >>= readString
  description <- readProp "description" f >>= readString
  keywords <- readProp "keywords" f >>= readArray >>= traverse readString
  nasaId <- readProp "nasa_id" f >>= readString
  pure $ SearchItem { title, description, keywords, nasaId }

readSearchResult :: Foreign -> F SearchResult
readSearchResult f = do
  href <- readProp "href" f >>= readString
  metadata <- readProp "metadata" f >>= readMetadata
  arr <- readProp "items" f >>= readArray
  items <- arr # traverse (readProp "data" >=> readSearchItem)
  pure $ SearchResult { href, metadata, items }
  where
    srch [f] = readSearchItem f
    srch _ = fail $ ForeignError "empty data field"
