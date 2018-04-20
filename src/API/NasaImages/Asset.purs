module API.NasaImages.Asset where

import Prelude

import Data.Record.ShowRecord (showRecord)

newtype Image dim = Image
  { url :: String
  , height :: dim
  , width :: dim
  }

instance showImage :: Show dim => Show (Image dim) where
  show (Image i) = showRecord i

newtype Asset dim = Asset
  { original :: Image dim
  , thumb :: String
  }

instance showAsset :: Show dim => Show (Asset dim) where
  show (Asset r) = showRecord r

withDimensions :: forall a b. { width :: b, height :: b } -> Asset a -> Asset b
withDimensions { width, height } (Asset a) =
  let
    Image img = a.original
  in Asset $ a { original = Image (img { width = width, height = height }) }
