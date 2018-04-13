module API.NasaImages.Validation where

import Prelude

import API.NasaImages (Metadata(..), SearchItem(..), SearchResult(..))
import Data.Argonaut (Json, foldJson, toArray, toNumber, toObject, toString)
import Data.Array (singleton, (!!))
import Data.Bifunctor (lmap)
import Data.Generic (class Generic, gShow)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Record.Fold (collect)
import Data.StrMap (StrMap, lookup)
import Data.Traversable (sequence, traverse)
import Polyform.Validation (V(..), Validation, hoistFnMV, hoistFnV, runValidation)

data JsError = JsError (List String) String
derive instance genericJsError :: Generic JsError
instance showJsError :: Show JsError where show = gShow


type JsValidation m a = Validation m (Array JsError) Json a

jsType :: Json -> String
jsType = foldJson
  (const "null")
  (const "bool")
  (const "number")
  (const "string")
  (const "array")
  (const "object")

fail :: forall a. String -> V (Array JsError) a
fail msg = Invalid $ singleton $ JsError Nil msg

int :: forall m. Monad m => JsValidation m Int
int = hoistFnV $ \v ->
  case toNumber v >>= fromNumber of
    Nothing -> fail (jsType v <> " is not an int")
    Just n -> pure n

object :: forall m. Monad m => Validation m (Array JsError) Json (StrMap Json)
object = hoistFnV $ \v ->
  case toObject v of
    Nothing -> fail (jsType v <> " is not an object")
    Just o -> pure o

string :: forall m. Monad m => JsValidation m String
string = hoistFnV $ \v ->
  case toString v of
    Nothing -> fail (jsType v <> " is not a string")
    Just s -> pure s

field :: forall m a. Monad m => String -> JsValidation m a -> JsValidation m a
field f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ fail ("no field " <> show f <> " in object " <> show v)
    Just json -> do
      res <- runValidation nested json
      pure $ lmap (map \(JsError p e) -> JsError (f : p) e) res)

optionalField
  :: forall m a
   . Monad m
  => Monoid a
  => String -> JsValidation m a -> JsValidation m a
optionalField f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ pure mempty
    Just json -> do
      res <- runValidation nested json
      pure $ lmap (map \(JsError p e) -> JsError (f : p) e) res)

array :: forall m a. Monad m => JsValidation m (Array Json)
array = hoistFnV $ \v ->
  case toArray v of
    Nothing -> fail (jsType v <> " is not an array")
    Just a -> pure a

metadata :: forall m. Monad m => JsValidation m Metadata
metadata = Metadata <$> (collect { "totalHits": field "total_hits" int })

elem :: forall m a. Monad m => Int -> JsValidation m a -> JsValidation m a
elem i v = array >>> hoistFnMV (\arr ->
  case arr !! i of
    Nothing -> pure $ fail ("no element at index " <> show i)
    Just a -> runValidation v a)

arrayOf :: forall m a. Monad m => JsValidation m a -> JsValidation m (Array a)
arrayOf v = array >>> hoistFnMV f
  where
    f = map sequence <<< traverse (runValidation v)

searchItem :: forall m. Monad m => JsValidation m SearchItem
searchItem = SearchItem <$> (collect
  { title: field "title" string
  , description: field "description" string
  , keywords: optionalField "keywords" $ arrayOf string
  , nasaId: field "nasa_id" string
  })

searchResult :: forall m. Monad m => JsValidation m SearchResult
searchResult = SearchResult <$> (collect
  { href: field "href" string
  , items: field "items" $ arrayOf (field "data" (elem 0 searchItem))
  , metadata: field "metadata" $ metadata
  })
