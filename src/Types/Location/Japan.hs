{-# LANGUAGE TemplateHaskell #-}

module Types.Location.Japan
  ( japanTZ
  ) where

import Data.Time.LocalTime.TimeZone.Olson.TH
import Data.Time.LocalTime.TimeZone.Series

japanTZ :: TimeZoneSeries
japanTZ = $(loadTZFile "/usr/share/zoneinfo/Asia/Tokyo")
