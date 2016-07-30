{-# LANGUAGE OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Pusher where

import Control.Lens

import Control.Monad.Reader
import Control.Monad.Trans.Except

import Data.List
import Data.List.Utils
import qualified Data.Text as T
import Data.Text.Lens
import Data.Time.LocalTime

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Types
import Types.Config
import Types.Location
import Pebble.Client
import Pebble.Types
import Utils


push :: AppM ()
push = do
    forecasts <- stateM $ use stForecasts
    let fcount = length forecasts
    manager <- liftIO $ newManager tlsManagerSettings
    logStr $ "Pushing " ++ show fcount ++ " forecasts..."
    mapM_ (putForecastPin manager) forecasts
    logStr "All pushed."

putForecastPin :: Manager -> Forecast -> AppM ()
putForecastPin manager forecast = do
    apiKey <- asks coApiKey
    let topics = forecastTopics forecast
    let pins = forecastPin forecast
    forM_ pins $ \pin -> do
        result <- liftIO $ runExceptT $ putSharedPin (Just apiKey) (Just topics) (pinId pin) pin manager pebbleUrl
        case result of
            Left err -> do
                logStr $ "Error pushing forecast: " ++ show err
                return ()
            Right _ -> return ()

forecastPin :: Forecast -> [Pin]
forecastPin fc = [ Pin { pinId = pinId `T.append` "start"
                       , pinTime = fcStartTimeUtc fc
                       , pinDuration = Nothing
                       , pinCreateNotification = Nothing
                       , pinUpdateNotification = Nothing
                       , pinLayout = startLayout
                       , pinReminders = []
                       , pinActions = []
                       }
                 , Pin { pinId = pinId `T.append` "end"
                       , pinTime = fcEndTimeUtc fc
                       , pinDuration = Nothing
                       , pinCreateNotification = Nothing
                       , pinUpdateNotification = Nothing
                       , pinLayout = endLayout
                       , pinReminders = []
                       , pinActions = []
                       }
                 ]
    where
        baseLayout = Layout { layoutType = GenericPin
                            , layoutTitle = ""
                            , layoutSubtitle = Just notificationTitle
                            , layoutBody = Just notificationText
                            , layoutTinyIcon = Nothing
                            , layoutSmallIcon = Nothing
                            , layoutLargeIcon = Nothing
                            , layoutPrimaryColor = Nothing
                            , layoutSecondaryColor = Nothing
                            , layoutBackgroundColor = Nothing
                            , layoutParagraphs = []
                            , layoutLastUpdated = Nothing
                            }
        notificationTitle = "Max level: " ++ show (fc ^. fcMaxLevel . uvValue)
        notificationText = "Alert from " ++ showTime (fc ^. fcAlertStart) ++ " to " ++ showTime (fc ^. fcAlertEnd)
        startLayout = baseLayout { layoutTitle = "UV Alert start"
                                 , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                                 }
        endLayout = baseLayout { layoutTitle = "UV Alert end"
                               , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                               }
        pinId = normalizeValue $ (fc ^. fcLocation . to locationId) `T.append` (fc ^. fcDate . to show . packed)

-- Initially the application only supported Australia, so city in the topic was
-- sufficient. Have to maintain for backwards compatibility
forecastTopics :: Forecast -> Topics
forecastTopics forecast = Topics $ [locTopic] ++ [legacyTopic | country == "Australia"]
    where legacyTopic = normalizeValue city
          locTopic = normalizeValue $ "v2-" `T.append` (forecast ^. fcLocation . to locationId)
          loc = forecast ^. fcLocation
          country = loc ^. locCountry . packed :: T.Text
          city = loc ^. locCity . packed

-- String to identify locations in pin IDs and topic names
locationId :: Location -> T.Text
locationId loc = T.intercalate "-" [country, region, city]
    where country = loc ^. locCountry . packed
          region = loc ^. locRegion . packed
          city = loc ^. locCity . packed

-- Replace characters not allowed in Pebble pin IDs and topic names
normalizeValue :: T.Text -> T.Text
normalizeValue = T.replace " " "_" . removeAccents

showTime :: TimeOfDay -> String
showTime = take 5 . show
