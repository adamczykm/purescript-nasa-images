module API.NasaImages.Validation where

import Prelude

import API.NasaImages.Asset (Asset(..), Image(..))
import API.NasaImages.Search (Item(..), Metadata(..), Request(..), Result(..))
import Control.Alt ((<|>))
import Data.Argonaut (Json, jsonParser)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Data.String (Pattern(..), contains)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnV, lmapValidation)
import Validators.Json (JsValidation, arrayOf, elem, fail, field, int, optionalField, string)

type AffjaxValidation m a = Validation m (Array String) (AffjaxResponse String) a
type AffjaxJson m = AffjaxValidation m Json

affjaxJson :: forall m. Monad m => AffjaxJson m
affjaxJson = parseJson <<< validateStatus
  where
  parseJson = hoistFnV $ \response -> case jsonParser response of
    Right json -> Valid [] json
    Left e -> Invalid ["Json parsing error: " <> e]
  validateStatus = hoistFnV $ \response -> case response.status of
    StatusCode 200 -> Valid [] response.response
    StatusCode s ->
      Invalid ["API bad response:\n" <> show s <> "\n" <> "\"" <> response.response <> "\""]

stringifyErrs
  :: forall m e a b
   . Show e
  => Monad m
  => Validation m (Array e) a b -> Validation m (Array String) a b
stringifyErrs = lmapValidation (map show)

metadata :: forall m. Monad m => JsValidation m Metadata
metadata = Metadata <$> (collect { "totalHits": field "total_hits" int })

searchItem :: forall m. Monad m => JsValidation m (Item String)
searchItem = Item <$> (
  { asset: _
  , description: _
  , keywords: _
  , nasaId: _
  , preview: _
  , title: _
  }
  <$> field "href" string
  <*> fromData "description" string
  <*> (field "data" $ elem 0 $ (optionalField "keywords" $ arrayOf string))
  <*> fromData "nasa_id" string
  <*> (Just <$> (field "links" $ elem 0 $ field "href" string) <|> pure Nothing)
  <*> fromData "title" string)
  where
  fromData n v = field "data" $ elem 0 $ field n v

these
  :: forall a b m
   . Monad m
  => JsValidation m a
  -> JsValidation m b
  -> JsValidation m (These a b)
these vA vB
  = Both <$> (elem 0 vA) <*> (elem 1 vB)
  <|> This <$> elem 0 vA
  <|> That <$> elem 0 vB

pagination :: forall m. Monad m => Request -> JsValidation m (These Request Request)
pagination (Request req) = these (page "prev" (_ - 1)) (page "next" (_ + 1))
  where
  page rel p = (Tuple <$> field "rel" string <*> field "href" string) >>> hoistFnV (case _ of
    (Tuple r h) | r == rel -> pure (Request req{ page = p req.page })
    (Tuple r _) -> fail ("Invalid " <> rel <> " link rel: " <> r))

searchResult :: forall m. Monad m => Request -> JsValidation m (Result String)
searchResult req = Result <$> (collect
  { href: field "href" string
  , items: field "items" $ arrayOf searchItem
  , pagination: field "links" (pagination req)
  , metadata: field "metadata" $ metadata
  })

dimensions :: forall m. Monad m => JsValidation m { width :: Int, height :: Int }
dimensions = { width: _, height: _ } 
  <$> ( field "EXIF:ImageWidth" int
    <|> field "File:ImageWidth" int
    <|> field "EXIF:ExifImageWidth" int)
  <*> ( field "EXIF:ImageHeight" int
    <|> field "File:ImageHeight" int
    <|> field "EXIF:ExifImageWidth" int)

findStr :: forall m. Monad m => String -> Validation m (Array String) (Array String) String
findStr s = hoistFnV $ \arr ->
  case find (contains $ Pattern s) arr of
    Nothing -> Invalid [ "Could not find string containing pattern: " <> s ]
    Just a -> pure a

asset :: forall m. Monad m => Validation m (Array String) (Array String) (Asset Unit)
asset = hoistFnV (\urls ->
  let
    a = do
      orig <- find (contains $ Pattern "orig") urls
      thumb <- find (contains $ Pattern "thumb") urls
      pure $ Asset { original: Image { url: orig, width : unit, height : unit }, thumb: thumb }
  in case a of
    Just a'  -> pure a'
    Nothing -> Invalid [ "Could not retrieve asset info" ])
