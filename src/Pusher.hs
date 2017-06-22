{-# LANGUAGE OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Pusher where

import Control.Lens

import Control.Monad.Reader
import Control.Monad.Trans.Except

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
forecastPin fc = do
    alert <- fc ^. fcAlerts
    let notificationTitle = "Max level: " ++ show (fc ^. fcMaxLevel . uvValue)
    let notificationText = "Alert from " ++ showTime (alert ^. alertStart) ++ " to " ++ showTime (alert ^. alertEnd)
    let baseLayout = Layout { layoutType = GenericPin
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
    let startLayout = baseLayout { layoutTitle = "UV Alert start"
                                 , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                                 }
    let endLayout = baseLayout { layoutTitle = "UV Alert end"
                               , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                               }
    let pinId = normalizeValue $ (fc ^. fcLocation . locId) `T.append` (fc ^. fcDate . to show . packed)
    [ Pin { pinId = pinId `T.append` "start"
                       , pinTime = fcAlertStartTime fc alert
                       , pinDuration = Nothing
                       , pinCreateNotification = Nothing
                       , pinUpdateNotification = Nothing
                       , pinLayout = startLayout
                       , pinReminders = []
                       , pinActions = []
                       }
                 , Pin { pinId = pinId `T.append` "end"
                       , pinTime = fcAlertEndTime fc alert
                       , pinDuration = Nothing
                       , pinCreateNotification = Nothing
                       , pinUpdateNotification = Nothing
                       , pinLayout = endLayout
                       , pinReminders = []
                       , pinActions = []
                       }
                 ]

-- Initially the application only supported Australia, so city in the topic was
-- sufficient. Have to maintain for backwards compatibility
forecastTopics :: Forecast -> Topics
forecastTopics forecast = Topics $ [locTopic] ++ [legacyTopic | country == "Australia"]
    where legacyTopic = normalizeValue city
          locTopic = "v2-" `T.append` (forecast ^. fcLocation . locId)
          loc = forecast ^. fcLocation
          country = loc ^. locCountry . packed :: T.Text
          city = loc ^. locCity . packed

showTime :: TimeOfDay -> String
showTime = take 5 . show
