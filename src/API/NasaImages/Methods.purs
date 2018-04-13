module API.NasaImages.Methods where

import Prelude

import API.NasaImages (SearchRequest, SearchResult, toUrlEncoded)
import API.NasaImages.Validation (searchResult)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Polyform.Validation (V, runValidation)
import Validators.Json (JsError, field)

buildRequest :: SearchRequest -> AffjaxRequest Unit
buildRequest r = 
  let url = r # toUrlEncoded # encode
  in defaultRequest { url = "https://images-api.nasa.gov/search?" <> url, method = Left GET }

search :: forall e. SearchRequest -> Aff (ajax :: AJAX | e) (V (Array JsError) SearchResult)
search r = do
  resp <- affjax (buildRequest r)
  runValidation (field "collection" searchResult) resp.response
