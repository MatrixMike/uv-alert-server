{-# LANGUAGE TemplateHaskell #-}

module Types.Location.USA
  ( central
  , pacific
  , eastern
  , mountain
  , alaska
  , hawaii
  ) where

import Data.Time.LocalTime.TimeZone.Olson.TH
import Data.Time.LocalTime.TimeZone.Series

central :: TimeZoneSeries
central = $(loadTZFile "/usr/share/zoneinfo/US/Central")

pacific :: TimeZoneSeries
pacific = $(loadTZFile "/usr/share/zoneinfo/US/Pacific")

eastern :: TimeZoneSeries
eastern = $(loadTZFile "/usr/share/zoneinfo/US/Eastern")

mountain :: TimeZoneSeries
mountain = $(loadTZFile "/usr/share/zoneinfo/US/Mountain")

alaska :: TimeZoneSeries
alaska = $(loadTZFile "/usr/share/zoneinfo/US/Alaska")

hawaii :: TimeZoneSeries
hawaii = $(loadTZFile "/usr/share/zoneinfo/US/Hawaii")
