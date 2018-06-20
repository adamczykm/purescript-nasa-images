module API.NasaImages.Methods where

import Prelude

import API.NasaImages.Asset (Asset, withDimensions)
import API.NasaImages.Search (Item(Item), Request, Result(Result), toUrlEncoded)
import API.NasaImages.Validation (SearchErrorRow, affjaxJson, asset, dimensions, findStr, getJson, searchResult)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Parallel (parTraverse)
import Data.Either (Either(..))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Record.Fold (rMap)
import Data.Traversable (sequence)
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Polyform.Validation (Validation, hoistFn, hoistFnMV, runValidation)
import Validators.Json (arrayOf, field, string)

buildRequest :: Request -> AffjaxRequest Unit
buildRequest r =
  let url = r # toUrlEncoded # encode
  in defaultRequest { url = "https://images-api.nasa.gov/search?" <> url, method = Left GET }

search :: forall e err. Validation (Aff (ajax :: AJAX | e)) (Array (Variant (SearchErrorRow err))) Request (Result String)
search = hoistFnMV $ \req -> runValidation
  (hoistFn buildRequest >>> affjaxJson >>> (field "collection" (searchResult req))) req

getDimensions
  :: forall e err
   . Validation 
      (Aff (ajax :: AJAX | e))  
      (Array (Variant (SearchErrorRow err))) 
      String { width :: Int, height :: Int }
getDimensions = (getJson >>> dimensions)

retrieve :: forall e err. Validation (Aff (ajax :: AJAX | e)) (Array (Variant (SearchErrorRow err))) String (Asset (Maybe Int))
retrieve = getJson >>> (arrayOf string) >>> (withDimensions
    <$> ((findStr "metadata" >>> getDimensions >>> hoistFn (rMap Just))
          <|> pure { width: Nothing, height: Nothing })
    <*> asset)

searchAndRetrieve
  :: forall e err
   . Validation 
      (Aff (ajax :: AJAX | e)) 
      (Array (Variant (SearchErrorRow err))) 
      Request 
      (Result (Asset (Maybe Int)))
searchAndRetrieve = search >>> (hoistFnMV $ \(Result r) -> do
  assets <- sequence <$> parTraverse (\(Item i) -> do
    asset <- runValidation retrieve i.asset
    pure $ Item <$> (i { asset = _ }) <$> asset) r.items
  pure $ (\arr -> Result $ r { items = arr }) <$> assets)
