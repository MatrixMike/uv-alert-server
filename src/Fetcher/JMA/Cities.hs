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
         -- Tokyo is a metropolitan area (都)
cities = [ (loc "Tokyo" "Tokyo", latlon 35.683333 139.683333)
         -- Cities designated by government ordnance (政令指定都市)
         , (loc "Aichi" "Nagoya", latlon 35.116665 136.93333)
         , (loc "Fukuoka" "Fukuoka", latlon 33.59028 130.40195)
         , (loc "Kyōto" "Kyoto", latlon 35.016666 135.75)
         , (loc "Hiroshima" "Hiroshima", latlon 34.385277 132.45528)
         , (loc "Ōsaka" "Ōsaka", latlon 34.69361 135.50194)
         , (loc "Ishikari" "Sapporo", latlon 43.05 141.35)
         , (loc "Kanagawa" "Yokohama", latlon 35.433334 139.63333)
         , (loc "Miyagi" "Sendai", latlon 38.268333 140.86945)
         , (loc "Hyōgo" "Kōbe", latlon 34.699722 135.14333)
         , (loc "Saitama" "Saitama", latlon 35.861668 139.64528)
         , (loc "Kanagawa" "Kawasaki", latlon 35.516666 139.7)
         , (loc "Chiba" "Chiba", latlon 35.6 140.1)
         , (loc "Shizuoka" "Shizuoka", latlon 34.975555 138.3825)
         , (loc "Niigata" "Niigata", latlon 37.91611 139.01973)
         , (loc "Shizuoka" "Hamamatsu", latlon 34.716667 137.73334)
         , (loc "Fukuoka" "Kitakyūshū", latlon 33.883335 130.88333)
         , (loc "Ōsaka" "Sakai", latlon 34.566666 135.48334)
         , (loc "Kumamoto" "Kumamoto", latlon 32.8 130.7)
         , (loc "Okayama" "Okayama", latlon 34.65 133.91667)
         , (loc "Kanagawa" "Sagamihara", latlon 35.566666 139.36667)
         -- Core cities (中核市)
         , (loc "Kagoshima" "Kagoshima", latlon 31.596666 130.55722)
         , (loc "Akita" "Akita", latlon 39.719723 140.1025)
         , (loc "Oshima" "Hakodate", latlon 41.773335 140.7261)
         , (loc "Nagasaki" "Nagasaki", latlon 32.783333 129.86667)
         , (loc "Gifu" "Gifu", latlon 35.416668 136.76666)
         , (loc "Nagano" "Nagano", latlon 36.648613 138.19278)
         , (loc "Aomori" "Aomori", latlon 40.822224 140.7475)
         , (loc "Nara" "Nara", latlon 34.683334 135.78334)
         , (loc "Okinawa" "Naha", latlon 26.212223 127.67889)
         , (loc "Ishikawa" "Kanazawa", latlon 36.566666 136.65)
         , (loc "Ōita" "Ōita", latlon 33.233334 131.60667)
         , (loc "Ehime" "Matsuyama", latlon 33.833332 132.76666)
         , (loc "Iwate" "Morioka", latlon 39.683334 141.15)
         , (loc "Kōchi" "Kōchi", latlon 33.566666 133.53334)
         , (loc "Kanagawa" "Yokosuka", latlon 35.25 139.66667)
         , (loc "Tochigi" "Utsunomiya", latlon 36.55 139.88333)
         , (loc "Kagawa" "Takamatsu", latlon 34.35 134.05)
         , (loc "Kamikawa" "Asahikawa", latlon 43.766666 142.36667)
         , (loc "Wakayama" "Wakayama", latlon 34.233334 135.16667)
         , (loc "Aichi" "Toyota", latlon 35.083332 137.15666)
         , (loc "Gunma" "Maebashi", latlon 36.383335 139.06667)
         , (loc "Shiga" "Ōtsu", latlon 35.016666 135.85)
         , (loc "Yamaguchi" "Shimonoseki", latlon 33.95 130.93333)
         , (loc "Toyama" "Toyama", latlon 36.7 137.22)
         , (loc "Miyazaki" "Miyazaki", latlon 31.916666 131.41667)
         , (loc "Hyōgo" "Himeji", latlon 34.816666 134.68333)
         , (loc "Hyōgo" "Amagasaki", latlon 34.733334 135.4)
         , (loc "Aichi" "Okazaki", latlon 34.95 137.16667)
         , (loc "Hyōgo" "Nishinomiya", latlon 34.733334 135.33333)
         , (loc "Ōsaka" "Higashiōsaka", latlon 34.679443 135.60083)
         , (loc "Fukushima" "Kōriyama", latlon 37.4 140.38333)
         , (loc "Chiba" "Funabashi", latlon 35.69472 139.9825)
         , (loc "Fukushima" "Iwaki", latlon 37.033333 140.88333)
         , (loc "Saitama" "Kawagoe", latlon 35.933334 139.48334)
         , (loc "Chiba" "Kashiwa", latlon 35.854443 139.96889)
         , (loc "Fukuoka" "Kurume", latlon 33.316666 130.51666)
         , (loc "Ōsaka" "Takatsuki", latlon 34.85 135.61667)
         , (loc "Aichi" "Toyohashi", latlon 34.766666 137.38333)
         , (loc "Gunma" "Takasaki", latlon 36.316666 139.0)
         , (loc "Ōsaka" "Toyonaka", latlon 34.783333 135.46666)
         , (loc "Okayama" "Kurashiki", latlon 34.585 133.77194)
         , (loc "Hiroshima" "Fukuyama", latlon 34.485832 133.3625)
         -- Special cities (特例市)
         , (loc "Aichi" "Kasugai", latlon 35.25 136.96666)
         , (loc "Hyōgo" "Akashi", latlon 34.643055 134.9975)
         , (loc "Mie" "Yokkaichi", latlon 34.966667 136.61667)
         , (loc "Saitama" "Kumagaya", latlon 36.14722 139.38861)
         , (loc "Saitama" "Tokorozawa", latlon 35.799446 139.46889)
         , (loc "Aomori" "Hachinohe", latlon 40.512222 141.48833)
         , (loc "Tottori" "Tottori", latlon 35.50111 134.235)
         , (loc "Fukui" "Fukui", latlon 36.064167 136.21973)
         , (loc "Yamanashi" "Kōfu", latlon 35.666668 138.56667)
         , (loc "Ibaraki" "Mito", latlon 36.366665 140.46666)
         , (loc "Yamagata" "Yamagata", latlon 38.25 140.33333)
         , (loc "Shimane" "Matsue", latlon 35.468056 133.04861)
         , (loc "Nagano" "Matsumoto", latlon 36.238056 137.97194)
         , (loc "Hiroshima" "Kure", latlon 34.24917 132.56583)
         , (loc "Shizuoka" "Numazu", latlon 35.095554 138.86362)
         , (loc "Kanagawa" "Odawara", latlon 35.26472 139.15222)
         , (loc "Aichi" "Ichinomiya", latlon 35.35 136.76666)
         , (loc "Ōsaka" "Hirakata", latlon 34.816666 135.65)
         , (loc "Ibaraki" "Tsukuba", latlon 36.033333 140.06667)
         , (loc "Nagasaki" "Sasebo", latlon 33.18 129.71556)
         , (loc "Gunma" "Isesaki", latlon 36.316666 139.2)
         , (loc "Shizuoka" "Fuji", latlon 35.16139 138.67639)
         , (loc "Saitama" "Kasukabe", latlon 35.975277 139.7525)
         , (loc "Gunma" "Ōta", latlon 36.291668 139.37584)
         , (loc "Kanagawa" "Yamato", latlon 35.4875 139.45805)
         , (loc "Saitama" "Kawaguchi", latlon 35.807777 139.72417)
         , (loc "Ōsaka" "Neyagawa", latlon 34.76611 135.62805)
         , (loc "Kanagawa" "Atsugi", latlon 35.433334 139.36667)
         , (loc "Ōsaka" "Yao", latlon 34.626945 135.60083)
         , (loc "Saitama" "Koshigaya", latlon 35.89111 139.79083)
         , (loc "Kanagawa" "Hiratsuka", latlon 35.323055 139.34222)
         , (loc "Ōsaka" "Ibaraki", latlon 34.816387 135.5686)
         , (loc "Niigata" "Nagaoka", latlon 37.433334 138.83333)
         , (loc "Niigata" "Jōetsu", latlon 37.15 138.23334)
         , (loc "Kanagawa" "Chigasaki", latlon 35.33389 139.40472)
         , (loc "Ōsaka" "Suita", latlon 34.766666 135.51666)
         , (loc "Saitama" "Sōka", latlon 35.825554 139.80556)
         , (loc "Ōsaka" "Kishiwada", latlon 34.460278 135.37111)
         , (loc "Hyōgo" "Kakogawa", latlon 34.756943 134.84138)
         , (loc "Hyōgo" "Takarazuka", latlon 34.8 135.36028)
         -- Cities (市)
         -- Too many of them and probably close to big cities anyway
         ]
             where loc = Location "Japan"
