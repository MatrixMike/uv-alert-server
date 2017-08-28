module Fetcher.Australia.Common
  ( auCities
  ) where

import Types.Location

auCities :: [LocationCoordinates]
auCities =
  map
    makeLocation
    [ (sa, "Adelaide", -34.92, 138.62)
    , (nt, "Alice Springs", -23.7, 133.8)
    , (qld, "Brisbane", -27.45, 153.03)
    , (act, "Canberra", -35.31, 149.2)
    , (nt, "Darwin", -12.43, 130.89)
    , (tas, "Kingston", -42.99, 147.29)
    , (vic, "Melbourne", -37.73, 145.1)
    , (nsw, "Newcastle", -32.9, 151.72)
    , (wa, "Perth", -31.92, 115.96)
    , (nsw, "Sydney", -34.04, 151.1)
    , (qld, "Townsville", -19.33, 146.76)
    ]
  where
    makeLocation (state, town, lat, lon) =
      Location "Australia" state town $ latlon lat lon
    act = "Australian Capital Territory"
    nsw = "New South Wales"
    nt = "Northern Territory"
    qld = "Queensland"
    sa = "South Australia"
    tas = "Tasmania"
    vic = "Victoria"
    wa = "Western Australia"
