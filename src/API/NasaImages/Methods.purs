module API.NasaImages.Methods where

import Prelude

import API.NasaImages.Asset (Asset)
import API.NasaImages.Search (Item(..), Request, Result(..), toUrlEncoded)
import API.NasaImages.Validation (asset, searchResult, stringifyErrs)
import Control.Monad.Aff (Aff)
import Control.Parallel (parTraverse)
import Data.Either (Either(..))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Traversable (sequence)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest, get)
import Polyform.Validation (Validation, hoistFnMV, runValidation)
import Validators.Json (field)

buildRequest :: Request -> AffjaxRequest Unit
buildRequest r = 
  let url = r # toUrlEncoded # encode
  in defaultRequest { url = "https://images-api.nasa.gov/search?" <> url, method = Left GET }

search :: forall e. Validation (Aff (ajax :: AJAX | e)) (Array String) Request (Result String)
search = hoistFnMV $ \r -> do
  resp <- affjax (buildRequest r)
  runValidation (stringifyErrs $ field "collection" searchResult) resp.response

retrieve :: forall e. Validation (Aff (ajax :: AJAX | e)) (Array String) String (Asset Unit)
retrieve = hoistFnMV $ \url -> do
  resp <- get url
  runValidation asset resp.response

searchAndRetrieve :: forall e. Validation (Aff (ajax :: AJAX | e)) (Array String) Request (Result (Asset Unit))
searchAndRetrieve = search >>> (hoistFnMV $ \(Result r) -> do
  assets <- sequence <$> parTraverse (\(Item i) -> do
    asset <- runValidation retrieve i.collection
    pure $ Item <$> (i { collection = _ }) <$> asset) r.items
  pure $ (\arr -> Result $ r { items = arr }) <$> assets
)
