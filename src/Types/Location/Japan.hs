{-# LANGUAGE TemplateHaskell #-}
module Types.Location.Japan (
    japanTZ,
) where

import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson.TH


japanTZ :: TimeZoneSeries
japanTZ = $(loadTZFile "/usr/share/zoneinfo/Asia/Tokyo")
