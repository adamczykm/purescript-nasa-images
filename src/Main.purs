module Main where

import Prelude

import API.NasaImages (SearchRequest(..))
import API.NasaImages.Methods (search)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (logShow)
import Data.Maybe (Maybe(..))
import Polyform.Validation (V(..))

example :: SearchRequest
example = SearchRequest
  { q : Just "apollo 11"
  , description : Just "moon landing"
  , keywords : []
  }

main :: _
main = launchAff $ do
  v <- search example
  case v of
    Invalid e -> logShow e
    Valid _ v -> logShow v
