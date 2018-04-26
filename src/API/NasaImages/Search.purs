module API.NasaImages.Search where

import Prelude

import Data.Foldable (intercalate)
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Record.ShowRecord (showRecord)
import Data.These (These)
import Data.Tuple (Tuple(..))

newtype Request = Request
  { query :: Maybe String
  , description :: Maybe String
  , keywords :: Array String
  }

newtype Result c = Result
  { href :: String
  , items :: Array (Item c)
  , pagination ∷ These String String
  , metadata :: Metadata
  }

newtype Metadata = Metadata
  { totalHits :: Int
  }

newtype Item c = Item
  { title :: String
  , description :: String
  , keywords :: Array String
  , nasaId :: String
  , asset :: c
  }

derive instance newtypeItem :: Newtype (Item c) _
instance showSearchItem :: Show c => Show (Item c) where show = unwrap >>> showRecord

derive instance newtypeMetadata :: Newtype Metadata _
instance showMetadata :: Show Metadata where show = unwrap >>> showRecord

derive instance newtypeResult :: Newtype (Result c) _
instance showSearchResult :: Show c => Show (Result c) where show = unwrap >>> showRecord

toUrlEncoded :: Request -> FormURLEncoded
toUrlEncoded (Request req) = fromArray
  [ Tuple "q" req.query
  , Tuple "description" req.description
  , Tuple "keywords" (case intercalate "," req.keywords of
    "" -> Nothing
    s  -> Just s)
  , Tuple "media_type" $ Just "image"
  ]
