module Main where

import Prelude

import API.NasaImages (SearchRequest(..), toUrlEncoded)
import API.NasaImages.Validation (field, searchResult)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (logShow)
import Data.Either (Either(..))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, affjax, defaultRequest)
import Polyform.Validation (V(..), runValidation)

buildRequest :: forall e. SearchRequest -> AffjaxRequest Unit
buildRequest r = 
  let
    url = r # toUrlEncoded # encode
  in defaultRequest { url = "https://images-api.nasa.gov/search?" <> url, method = Left GET }

example = SearchRequest
  { q : Just "apollo 11"
  , description : Just "moon landing"
  , keywords : []
  }

main :: _
main = launchAff $ do
  resp <- affjax (buildRequest example)
  let f = resp.response
  v <- runValidation (field "collection" searchResult) f
  case v of
    Invalid e -> logShow e
    Valid _ v -> logShow v
