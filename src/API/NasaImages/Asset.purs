module API.NasaImages.Asset where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Image dim = Image
  { url :: String
  , height :: dim
  , width :: dim
  }
derive instance genericImage :: Generic (Image dim) _
instance showImage :: Show dim => Show (Image dim) where show = genericShow

newtype Asset dim = Asset
  { original :: Image dim
  , thumb :: String
  }
derive instance genericAsset :: Generic (Asset dim) _
instance showAsset :: Show dim => Show (Asset dim) where show = genericShow

withDimensions :: forall a b. { width :: b, height :: b } -> Asset a -> Asset b
withDimensions { width, height } (Asset a) =
  let
    Image img = a.original
  in Asset $ a { original = Image (img { width = width, height = height }) }
