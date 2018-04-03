module Main where

import Prelude

import API.NasaImages (SearchRequest(..), toUrlEncoded)
import API.NasaImages.Validation (readSearchResult)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept, runExceptT)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Index (readProp)
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Debug.Trace (traceAny, traceAnyA)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, affjax, defaultRequest)

buildRequest :: forall e. SearchRequest -> AffjaxRequest Unit
buildRequest r = 
  let
    url = r # toUrlEncoded # encode
  in traceAny url \_ -> defaultRequest { url = "https://images-api.nasa.gov/search?" <> url, method = Left GET }

example = SearchRequest
  { q : Just "apollo 11"
  , description : Just "moon landing"
  , keywords : []
  }

main :: _
main = launchAff $ do
  resp <- affjax (buildRequest example)
  let f = resp.response
  case runExcept $ readProp "collection" f >>= readSearchResult of
    Left e -> logShow e
    Right v -> logShow v
