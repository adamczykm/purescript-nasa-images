module API.NasaImages where

import Prelude

import Data.Foldable (intercalate)
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype SearchRequest = SearchRequest
  { q :: Maybe String
  , description :: Maybe String
  , keywords :: Array String
  }

newtype SearchResult = SearchResult
  { href :: String
  , items :: Array SearchItem
  , metadata :: Metadata
  }

newtype Metadata = Metadata
  { totalHits :: Int
  }

newtype SearchItem = SearchItem
  { title :: String
  , description :: String
  , keywords :: Array String
  , nasaId :: String
  , collection :: String
  }

derive instance genericSearchItem :: Generic SearchItem
instance showSearchItem :: Show SearchItem where show = gShow

derive instance genericMetadata :: Generic Metadata
instance showMetadata :: Show Metadata where show = gShow

derive instance genericSearchResult :: Generic SearchResult
instance showSearchResult :: Show SearchResult where show = gShow

toUrlEncoded :: SearchRequest -> FormURLEncoded
toUrlEncoded (SearchRequest req) = fromArray
  [ Tuple "q" req.q
  , Tuple "description" req.description
  , Tuple "keywords" (case intercalate "," req.keywords of
    "" -> Nothing
    s  -> Just s)
  , Tuple "media_type" $ Just "image"
  ]
