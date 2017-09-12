{-# LANGUAGE TemplateHaskell #-}

module Types.Location.TimeZones
  ( actTZ
  , nswTZ
  , ntTZ
  , qldTZ
  , saTZ
  , tasTZ
  , vicTZ
  , waTZ
  , auStateTZ
  , japanTZ
  , usCentral
  , usPacific
  , usEastern
  , usMountain
  , usAlaska
  , usHawaii
  ) where

import Data.Time.LocalTime.TimeZone.Olson.TH
import Data.Time.LocalTime.TimeZone.Series

actTZ :: TimeZoneSeries
actTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/Canberra")

nswTZ :: TimeZoneSeries
nswTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/NSW")

ntTZ :: TimeZoneSeries
ntTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/North")

qldTZ :: TimeZoneSeries
qldTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/Queensland")

saTZ :: TimeZoneSeries
saTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/South")

tasTZ :: TimeZoneSeries
tasTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/Tasmania")

vicTZ :: TimeZoneSeries
vicTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/Victoria")

waTZ :: TimeZoneSeries
waTZ = $(loadTZFile "/usr/share/zoneinfo/Australia/West")

auStateTZ :: String -> TimeZoneSeries
auStateTZ state
  | state == act = actTZ
  | state == nsw = nswTZ
  | state == nt = ntTZ
  | state == qld = qldTZ
  | state == sa = saTZ
  | state == tas = tasTZ
  | state == vic = vicTZ
  | state == wa = waTZ
  | otherwise = error "invalid state"
  where
    act = "Australian Capital Territory"
    nsw = "New South Wales"
    nt = "Northern Territory"
    qld = "Queensland"
    sa = "South Australia"
    tas = "Tasmania"
    vic = "Victoria"
    wa = "Western Australia"

japanTZ :: TimeZoneSeries
japanTZ = $(loadTZFile "/usr/share/zoneinfo/Asia/Tokyo")

usCentral :: TimeZoneSeries
usCentral = $(loadTZFile "/usr/share/zoneinfo/US/Central")

usPacific :: TimeZoneSeries
usPacific = $(loadTZFile "/usr/share/zoneinfo/US/Pacific")

usEastern :: TimeZoneSeries
usEastern = $(loadTZFile "/usr/share/zoneinfo/US/Eastern")

usMountain :: TimeZoneSeries
usMountain = $(loadTZFile "/usr/share/zoneinfo/US/Mountain")

usAlaska :: TimeZoneSeries
usAlaska = $(loadTZFile "/usr/share/zoneinfo/US/Alaska")

usHawaii :: TimeZoneSeries
usHawaii = $(loadTZFile "/usr/share/zoneinfo/US/Hawaii")
