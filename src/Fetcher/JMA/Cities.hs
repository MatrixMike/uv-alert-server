module Fetcher.JMA.Cities
  ( cities
  , imageCoord
  , ImageCoord(..)
  , latlon
  ) where

import Control.Lens

import Types.Location
import Types.Location.Japan
import Utils

data ImageCoord = ImageCoord
  { icX :: Int
  , icY :: Int
  } deriving (Show, Eq)

-- Reference points for converting the latitude and longitude
-- | Iturup island northeast of Japan, northeastern point
iturupLatLon :: Coordinates
iturupLatLon = latlon 45.529597 148.863393

iturupCoo :: ImageCoord
iturupCoo = ImageCoord 493 14

-- | Yakushima island, southwestern point
yakushimaLatLon :: Coordinates
yakushimaLatLon = latlon 30.262214 130.430049

yakushimaCoo :: ImageCoord
yakushimaCoo = ImageCoord 154 323

imageCoord :: Coordinates -> ImageCoord
imageCoord coo = ImageCoord (round x) (round y)
  where
    lo = coo ^. longitude
    la = coo ^. latitude
    x = extrapolate (fromIntegral x1 :: Double, lo1) (fromIntegral x2, lo2) lo
    y = extrapolate (fromIntegral y1 :: Double, la1) (fromIntegral y2, la2) la
    lo1 = iturupLatLon ^. longitude
    la1 = iturupLatLon ^. latitude
    (ImageCoord x1 y1) = iturupCoo
    lo2 = yakushimaLatLon ^. longitude
    la2 = yakushimaLatLon ^. latitude
    (ImageCoord x2 y2) = yakushimaCoo

cities :: [LocationCoordinates]
cities =
  [ loc "Tokyo" "Tokyo" (latlon 35.68 139.77) japanTZ
  , loc "Kanagawa" "Yokohama" (latlon 35.433334 139.63333) japanTZ
  , loc "Ōsaka" "Ōsaka" (latlon 34.69361 135.50194) japanTZ
  , loc "Aichi" "Nagoya" (latlon 35.116665 136.93333) japanTZ
  , loc "Fukuoka" "Fukuoka" (latlon 33.59028 130.40195) japanTZ
  , loc "Hyōgo" "Kōbe" (latlon 34.6913 135.183) japanTZ
  , loc "Kanagawa" "Kawasaki" (latlon 35.516666 139.7) japanTZ
  , loc "Kyōto" "Kyoto" (latlon 35.011665 135.76805) japanTZ
  , loc "Saitama" "Saitama" (latlon 35.861668 139.64528) japanTZ
  , loc "Hiroshima" "Hiroshima" (latlon 34.385277 132.45528) japanTZ
  , loc "Miyagi" "Sendai" (latlon 38.268333 140.86945) japanTZ
  , loc "Chiba" "Chiba" (latlon 35.6 140.1) japanTZ
  , loc "Fukuoka" "Kitakyūshū" (latlon 33.883335 130.88333) japanTZ
  , loc "Ōsaka" "Sakai" (latlon 34.566666 135.48334) japanTZ
  , loc "Niigata" "Niigata" (latlon 37.91611 139.01973) japanTZ
  , loc "Shizuoka" "Hamamatsu" (latlon 34.716667 137.73334) japanTZ
  , loc "Kumamoto" "Kumamoto" (latlon 32.8 130.7) japanTZ
  , loc "Kanagawa" "Sagamihara" (latlon 35.566666 139.36667) japanTZ
  , loc "Okayama" "Okayama" (latlon 34.65 133.91667) japanTZ
  , loc "Shizuoka" "Shizuoka" (latlon 34.975555 138.3825) japanTZ
  , loc "Chiba" "Funabashi" (latlon 35.69472 139.9825) japanTZ
  , loc "Kagoshima" "Kagoshima" (latlon 31.596666 130.55722) japanTZ
  , loc "Saitama" "Kawaguchi" (latlon 35.807777 139.72417) japanTZ
  , loc "Tokyo" "Hachiōji" (latlon 35.666668 139.31667) japanTZ
  , loc "Hyōgo" "Himeji" (latlon 34.816666 134.68333) japanTZ
  , loc "Tochigi" "Utsunomiya" (latlon 36.55 139.88333) japanTZ
  , loc "Ehime" "Matsuyama" (latlon 33.833332 132.76666) japanTZ
  , loc "Ōsaka" "Higashiōsaka" (latlon 34.679443 135.60083) japanTZ
  , loc "Hyōgo" "Nishinomiya" (latlon 34.733334 135.33333) japanTZ
  , loc "Chiba" "Ichikawa" (latlon 35.721943 139.9311) japanTZ
  , loc "Chiba" "Matsudo" (latlon 35.787777 139.90306) japanTZ
  , loc "Ōita" "Ōita" (latlon 33.233334 131.60667) japanTZ
  , loc "Okayama" "Kurashiki" (latlon 34.585 133.77194) japanTZ
  , loc "Ishikawa" "Kanazawa" (latlon 36.566666 136.65) japanTZ
  , loc "Hiroshima" "Fukuyama" (latlon 34.485832 133.3625) japanTZ
  , loc "Hyōgo" "Amagasaki" (latlon 34.733334 135.4) japanTZ
  , loc "Kanagawa" "Fujisawa" (latlon 35.33917 139.4914) japanTZ
  , loc "Aichi" "Toyota" (latlon 35.083332 137.15666) japanTZ
  , loc "Nagasaki" "Nagasaki" (latlon 32.783333 129.86667) japanTZ
  , loc "Chiba" "Kashiwa" (latlon 35.854443 139.96889) japanTZ
  , loc "Kagawa" "Takamatsu" (latlon 34.35 134.05) japanTZ
  , loc "Toyama" "Toyama" (latlon 36.7 137.22) japanTZ
  , loc "Gifu" "Gifu" (latlon 35.416668 136.76666) japanTZ
  , loc "Ōsaka" "Hirakata" (latlon 34.816666 135.65) japanTZ
  , loc "Kanagawa" "Yokosuka" (latlon 35.25 139.66667) japanTZ
  , loc "Miyazaki" "Miyazaki" (latlon 31.916666 131.41667) japanTZ
  , loc "Ōsaka" "Toyonaka" (latlon 34.783333 135.46666) japanTZ
  , loc "Aichi" "Okazaki" (latlon 34.95 137.16667) japanTZ
  , loc "Aichi" "Ichinomiya" (latlon 35.35 136.76666) japanTZ
  , loc "Ōsaka" "Suita" (latlon 34.766666 135.51666) japanTZ
  , loc "Nagano" "Nagano" (latlon 36.648613 138.19278) japanTZ
  , loc "Aichi" "Toyohashi" (latlon 34.766666 137.38333) japanTZ
  , loc "Gunma" "Takasaki" (latlon 36.316666 139.0) japanTZ
  , loc "Wakayama" "Wakayama" (latlon 34.233334 135.16667) japanTZ
  , loc "Nara" "Nara" (latlon 34.683334 135.78334) japanTZ
  , loc "Saitama" "Kawagoe" (latlon 35.933334 139.48334) japanTZ
  , loc "Ōsaka" "Takatsuki" (latlon 34.85 135.61667) japanTZ
  , loc "Fukushima" "Iwaki" (latlon 37.033333 140.88333) japanTZ
  , loc "Saitama" "Tokorozawa" (latlon 35.799446 139.46889) japanTZ
  , loc "Saitama" "Koshigaya" (latlon 35.89111 139.79083) japanTZ
  , loc "Shiga" "Ōtsu" (latlon 35.016666 135.85) japanTZ
  , loc "Gunma" "Maebashi" (latlon 36.383335 139.06667) japanTZ
  , loc "Fukushima" "Kōriyama" (latlon 37.4 140.38333) japanTZ
  , loc "Kōchi" "Kōchi" (latlon 33.566666 133.53334) japanTZ
  , loc "Okinawa" "Naha" (latlon 26.212223 127.67889) japanTZ
  , loc "Akita" "Akita" (latlon 39.719723 140.1025) japanTZ
  , loc "Mie" "Yokkaichi" (latlon 34.966667 136.61667) japanTZ
  , loc "Aichi" "Kasugai" (latlon 35.25 136.96666) japanTZ
  , loc "Fukuoka" "Kurume" (latlon 33.316666 130.51666) japanTZ
  , loc "Iwate" "Morioka" (latlon 39.683334 141.15) japanTZ
  , loc "Hyōgo" "Akashi" (latlon 34.643055 134.9975) japanTZ
  , loc "Fukushima" "Fukushima" (latlon 37.760834 140.47333) japanTZ
  , loc "Aomori" "Aomori" (latlon 40.822224 140.7475) japanTZ
  , loc "Ōsaka" "Ibaraki" (latlon 34.816387 135.5686) japanTZ
  , loc "Mie" "Tsu" (latlon 34.718613 136.50555) japanTZ
  , loc "Chiba" "Ichihara" (latlon 35.498055 140.11555) japanTZ
  , loc "Niigata" "Nagaoka" (latlon 37.433334 138.83333) japanTZ
  , loc "Ibaraki" "Mito" (latlon 36.366665 140.46666) japanTZ
  , loc "Ōsaka" "Yao" (latlon 34.626945 135.60083) japanTZ
  , loc "Hyōgo" "Kakogawa" (latlon 34.756943 134.84138) japanTZ
  , loc "Yamaguchi" "Shimonoseki" (latlon 33.95 130.93333) japanTZ
  , loc "Fukui" "Fukui" (latlon 36.064167 136.21973) japanTZ
  , loc "Tokyo" "Fuchū" (latlon 35.668888 139.47778) japanTZ
  , loc "Kanagawa" "Hiratsuka" (latlon 35.323055 139.34222) japanTZ
  , loc "Tokushima" "Tokushima" (latlon 34.066666 134.55) japanTZ
  , loc "Nagasaki" "Sasebo" (latlon 33.18 129.71556) japanTZ
  , loc "Yamagata" "Yamagata" (latlon 38.25 140.33333) japanTZ
  , loc "Saitama" "Sōka" (latlon 35.825554 139.80556) japanTZ
  , loc "Shizuoka" "Fuji" (latlon 35.16139 138.67639) japanTZ
  , loc "Nagano" "Matsumoto" (latlon 36.238056 137.97194) japanTZ
  , loc "Kanagawa" "Chigasaki" (latlon 35.33389 139.40472) japanTZ
  , loc "Ōsaka" "Neyagawa" (latlon 34.76611 135.62805) japanTZ
  , loc "Saga" "Saga" (latlon 33.266666 130.3) japanTZ
  , loc "Kanagawa" "Yamato" (latlon 35.4875 139.45805) japanTZ
  , loc "Saitama" "Kasukabe" (latlon 35.975277 139.7525) japanTZ
  , loc "Ibaraki" "Tsukuba" (latlon 36.033333 140.06667) japanTZ
  , loc "Aomori" "Hachinohe" (latlon 40.512222 141.48833) japanTZ
  , loc "Saitama" "Ageo" (latlon 35.9775 139.59334) japanTZ
  , loc "Hyōgo" "Takarazuka" (latlon 34.8 135.36028) japanTZ
  , loc "Kanagawa" "Atsugi" (latlon 35.433334 139.36667) japanTZ
  , loc "Hiroshima" "Kure" (latlon 34.24917 132.56583) japanTZ
  , loc "Gunma" "Ōta" (latlon 36.291668 139.37584) japanTZ
  , loc "Gunma" "Isesaki" (latlon 36.316666 139.2) japanTZ
  , loc "Shimane" "Matsue" (latlon 35.468056 133.04861) japanTZ
  , loc "Tokyo" "Nishitōkyō" (latlon 35.725834 139.5386) japanTZ
  , loc "Saitama" "Kumagaya" (latlon 36.14722 139.38861) japanTZ
  , loc "Yamaguchi" "Yamaguchi" (latlon 34.178055 131.47389) japanTZ
  , loc "Hyōgo" "Itami" (latlon 34.78417 135.40083) japanTZ
  , loc "Mie" "Suzuka" (latlon 34.88222 136.58417) japanTZ
  , loc "Chiba" "Yachiyo" (latlon 35.7225 140.09972) japanTZ
  , loc "Niigata" "Jōetsu" (latlon 37.15 138.23334) japanTZ
  , loc "Ōsaka" "Kishiwada" (latlon 34.460278 135.37111) japanTZ
  , loc "Hiroshima" "Higashihiroshima" (latlon 34.416668 132.73334) japanTZ
  , loc "Kanagawa" "Odawara" (latlon 35.26472 139.15222) japanTZ
  , loc "Shizuoka" "Numazu" (latlon 35.095554 138.86362) japanTZ
  , loc "Tottori" "Tottori" (latlon 35.50111 134.235) japanTZ
  , loc "Yamanashi" "Kōfu" (latlon 35.666668 138.56667) japanTZ
  , loc "Aichi" "Anjō" (latlon 34.966667 137.08333) japanTZ
  , loc "Ōsaka" "Izumi" (latlon 34.483612 135.42361) japanTZ
  , loc "Kyōto" "Uji" (latlon 34.880833 135.77945) japanTZ
  , loc "Aichi" "Toyokawa" (latlon 34.833332 137.38333) japanTZ
  , loc "Ibaraki" "Hitachi" (latlon 36.6 140.65) japanTZ
  , loc "Chiba" "Nagareyama" (latlon 35.85611 139.9025) japanTZ
  , loc "Aomori" "Hirosaki" (latlon 40.603054 140.46417) japanTZ
  , loc "Kanagawa" "Kamakura" (latlon 35.315834 139.55028) japanTZ
  , loc "Chiba" "Sakura" (latlon 35.72389 140.22389) japanTZ
  , loc "Shimane" "Izumo" (latlon 35.368057 132.755) japanTZ
  , loc "Chiba" "Narashino" (latlon 35.680832 140.02667) japanTZ
  , loc "Toyama" "Takaoka" (latlon 36.75 137.03334) japanTZ
  , loc "Aichi" "Nishio" (latlon 34.866665 137.06667) japanTZ
  , loc "Yamaguchi" "Ube" (latlon 33.951668 131.24667) japanTZ
  , loc "Chiba" "Urayasu" (latlon 35.65361 139.90167) japanTZ
  , loc "Shizuoka" "Iwata" (latlon 34.717777 137.8514) japanTZ
  , loc "Tochigi" "Oyama" (latlon 36.314724 139.80028) japanTZ
  , loc "Kanagawa" "Hadano" (latlon 35.37472 139.22028) japanTZ
  , loc "Saitama" "Niiza" (latlon 35.793335 139.56528) japanTZ
  , loc "Miyazaki" "Miyakonojō" (latlon 31.719723 131.06166) japanTZ
  , loc "Mie" "Matsusaka" (latlon 34.578056 136.5275) japanTZ
  , loc "Gifu" "Ōgaki" (latlon 35.359444 136.61278) japanTZ
  , loc "Tochigi" "Tochigi" (latlon 36.38139 139.73027) japanTZ
  , loc "Ehime" "Imabari" (latlon 34.066113 132.99777) japanTZ
  , loc "Ibaraki" "Hitachinaka" (latlon 36.39639 140.53444) japanTZ
  , loc "Nagano" "Ueda" (latlon 36.401943 138.24916) japanTZ
  , loc "Hyōgo" "Kawanishi" (latlon 34.83 135.41722) japanTZ
  , loc "Chiba" "Noda" (latlon 35.955 139.87473) japanTZ
  , loc "Saitama" "Kuki" (latlon 36.06222 139.66695) japanTZ
  , loc "Saitama" "Sayama" (latlon 35.853054 139.41222) japanTZ
  , loc "Aichi" "Kariya" (latlon 34.983334 137.0) japanTZ
  , loc "Aichi" "Komaki" (latlon 35.283333 136.91667) japanTZ
  , loc "Tottori" "Yonago" (latlon 35.428055 133.33112) japanTZ
  , loc "Tochigi" "Ashikaga" (latlon 36.34028 139.44972) japanTZ
  , loc "Saitama" "Iruma" (latlon 35.835835 139.39111) japanTZ
  , loc "Miyagi" "Ishinomaki" (latlon 38.428055 141.3061) japanTZ
  , loc "Gifu" "Kakamigahara" (latlon 35.398888 136.84862) japanTZ
  , loc "Shizuoka" "Fujieda" (latlon 34.8675 138.25778) japanTZ
  , loc "Yamaguchi" "Shūnan" (latlon 34.05528 131.8061) japanTZ
  , loc "Saitama" "Fukaya" (latlon 36.1975 139.28139) japanTZ
  , loc "Ōsaka" "Moriguchi" (latlon 34.7375 135.56416) japanTZ
  , loc "Okinawa" "Okinawa" (latlon 26.334167 127.80556) japanTZ
  , loc "Ibaraki" "Koga" (latlon 36.183334 139.7) japanTZ
  , loc "Mie" "Kuwana" (latlon 35.06222 136.68388) japanTZ
  , loc "Ibaraki" "Tsuchiura" (latlon 36.066666 140.2) japanTZ
  , loc "Saitama" "Toda" (latlon 35.8175 139.67778) japanTZ
  , loc "Shiga" "Kusatsu" (latlon 35.016666 135.96666) japanTZ
  , loc "Saitama" "Misato" (latlon 35.830276 139.8725) japanTZ
  , loc "Saitama" "Asaka" (latlon 35.797222 139.59361) japanTZ
  , loc "Shizuoka" "Yaizu" (latlon 34.866665 138.31667) japanTZ
  , loc "Nagasaki" "Isahaya" (latlon 32.844166 130.0536) japanTZ
  , loc "Aichi" "Inazawa" (latlon 35.25 136.78334) japanTZ
  , loc "Hiroshima" "Onomichi" (latlon 34.40889 133.205) japanTZ
  , loc "Ōsaka" "Minō" (latlon 34.826946 135.47055) japanTZ
  , loc "Chiba" "Kisarazu" (latlon 35.37611 139.91695) japanTZ
  , loc "Yamaguchi" "Iwakuni" (latlon 34.166943 132.21973) japanTZ
  , loc "Miyagi" "Ōsaki" (latlon 38.57722 140.95555) japanTZ
  , loc "Chiba" "Narita" (latlon 35.783333 140.31667) japanTZ
  , loc "Chiba" "Abiko" (latlon 35.864166 140.02834) japanTZ
  , loc "Kanagawa" "Ebina" (latlon 35.44639 139.39084) japanTZ
  , loc "Shizuoka" "Fujinomiya" (latlon 35.22222 138.62138) japanTZ
  , loc "Kanagawa" "Zama" (latlon 35.48861 139.4075) japanTZ
  , loc "Aichi" "Seto" (latlon 35.216667 137.08333) japanTZ
  , loc "Fukuoka" "Iizuka" (latlon 33.645832 130.69139) japanTZ
  , loc "Yamagata" "Tsuruoka" (latlon 38.72167 139.82167) japanTZ
  , loc "Mie" "Ise" (latlon 34.483334 136.71666) japanTZ
  , loc "Kumamoto" "Yatsushiro" (latlon 32.517776 130.61806) japanTZ
  , loc "Kagoshima" "Kirishima" (latlon 31.741112 130.76306) japanTZ
  , loc "Nara" "Kashihara" (latlon 34.509167 135.7925) japanTZ
  , loc "Tokyo" "Koganei" (latlon 35.699444 139.50305) japanTZ
  , loc "Miyazaki" "Nobeoka" (latlon 32.582222 131.665) japanTZ
  , loc "Ōsaka" "Kadoma" (latlon 34.733334 135.58333) japanTZ
  , loc "Fukushima" "Aizuwakamatsu" (latlon 37.49472 139.92972) japanTZ
  , loc "Ōsaka" "Daitō" (latlon 34.711945 135.62334) japanTZ
  ]
  where
    loc = Location "Japan"
