module API.NasaImages.Validation where

import Prelude

import API.NasaImages.Asset (Asset(..), Image(..))
import API.NasaImages.Search (Item(..), Metadata(..), Result(..))
import Data.Argonaut (Json)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Record (insert)
import Data.Record.Fold (collect)
import Data.String (Pattern(..), contains)
import Data.Symbol (SProxy(..))
import Polyform.Validation (V(..), Validation, hoistFnV, lmapValidation)
import Validators.Json (JsValidation, arrayOf, elem, field, int, optionalField, string)

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
  insert (SProxy :: SProxy "collection") <$>
    (field "href" string) <*>
    (field "data" $ elem 0 $ collect
      { title: field "title" string
      , description: field "description" string
      , keywords: optionalField "keywords" $ arrayOf string
      , nasaId: field "nasa_id" string
      }))

searchResult :: forall m. Monad m => JsValidation m (Result String)
searchResult = Result <$> (collect
  { href: field "href" string
  , items: field "items" $ arrayOf searchItem
  , metadata: field "metadata" $ metadata
  })

asset :: forall m. Monad m => Validation m (Array String) Json (Asset Unit)
asset = (arrayOf string # stringifyErrs) >>> hoistFnV (\urls -> 
  let 
    asset = do
      orig <- find (contains $ Pattern "orig") urls
      thumb <- find (contains $ Pattern "thumb") urls
      pure $ Asset { original: Image { url: orig, width : unit, height : unit }, thumb: thumb }
  in case asset of
    Just a  -> pure a
    Nothing -> Invalid [ "Could not retrieve asset info" ])
