module Main where

import Prelude

import API.NasaImages.Methods (retrieve, search, searchAndRetrieve)
import API.NasaImages.Search (Item(..), Request(..), Result(..))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (logShow)
import Control.Parallel (parTraverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Polyform.Validation (V(..), runValidation)

example :: Request
example = Request
  { query : Just "apollo 11"
  , description : Just "moon landing"
  , keywords : []
  }

main :: _
main = launchAff $ do
  v <- runValidation searchAndRetrieve example
  case v of
    Invalid e -> logShow e
    Valid _ r -> logShow r
