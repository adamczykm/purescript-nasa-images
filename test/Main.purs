module Test.Main where

import Prelude

import API.NasaImages.Methods (searchAndRetrieve)
import API.NasaImages.Search (Request(..))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log, logShow)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Global.Unsafe (unsafeStringify)
import Polyform.Validation (V(..), runValidation)

example :: Request
example = Request
  { query : Just "apollo 11"
  , description : Just "moon landing"
  , page : 1
  , keywords : []
  }

main :: _
main = launchAff $ do
  v <- runValidation searchAndRetrieve example
  case v of
    Invalid e -> log $ unsafeStringify e
    Valid _ r -> do
      logShow r
      logShow (length $ (unwrap r).items)
      logShow ((unwrap r).metadata)
