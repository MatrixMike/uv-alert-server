module Fetcher.JMA.Cities (
    cities,
    imageCoord,
    ImageCoord(..),
    LonLat(..),
    latlon,
) where

import Types.Location
import Utils


data ImageCoord = ImageCoord { icX :: Int, icY :: Int }
    deriving (Show, Eq)

data LonLat = LonLat { lon :: Float, lat :: Float }
    deriving (Show, Eq)

latlon = flip LonLat

-- Reference points for converting the latitude and longitude

-- Iturup island northeast of Japan, northeastern point
iturupLatLon = latlon 45.529597 148.863393
iturupCoo = ImageCoord 493 14

-- Yakushima island, southwestern point
yakushimaLatLon = latlon 30.262214 130.430049
yakushimaCoo = ImageCoord 154 323

imageCoord :: LonLat -> ImageCoord
imageCoord (LonLat lo la) = ImageCoord (round x) (round y)
    where x = extrapolate (lo1, fromIntegral x1) (lo2, fromIntegral x2) lo
          y = extrapolate (la1, fromIntegral y1) (la2, fromIntegral y2) la
          (LonLat lo1 la1) = iturupLatLon
          (ImageCoord x1 y1) = iturupCoo
          (LonLat lo2 la2) = yakushimaLatLon
          (ImageCoord x2 y2) = yakushimaCoo

cities :: [(Location, LonLat)]
cities = [ (loc "Tokyo" "Tokyo", latlon 35.683333 139.683333)
         , (loc "Hiroshima" "Hiroshima", latlon 34.385278 132.455278)
         , (loc "Hokkaido" "Sapporo", latlon 43.066667 141.35)
         ]
             where loc = Location "Japan"
