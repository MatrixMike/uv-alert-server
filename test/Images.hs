module Images where

{- Load images for testing -}

import Codec.Picture

import qualified Data.ByteString as BS

import Test.Hspec
import Test.Hspec.Core.Spec


loadImage :: String -> SpecM a DynamicImage
loadImage imageName = do
    bytes <- runIO $ BS.readFile $ "test/" ++ imageName
    let (Right image) = decodeImage bytes
    return image
