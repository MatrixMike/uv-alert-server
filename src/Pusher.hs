{-# Language RecordWildCards #-}
module Pusher where

import Control.Lens

import Control.Monad.Reader
import Control.Monad.Trans.Except

import Data.List
import Data.List.Utils
import Data.Time.LocalTime

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import App
import Types
import Types.Location
import Pebble.Client
import Pebble.Types


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
forecastPin fc = [ Pin { pinId = pinId ++ "start"
                       , pinTime = fcStartTimeUtc fc
                       , pinDuration = Nothing
                       , pinCreateNotification = Nothing
                       , pinUpdateNotification = Nothing
                       , pinLayout = startLayout
                       , pinReminders = []
                       , pinActions = []
                       }
                 , Pin { pinId = pinId ++ "end"
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
        pinId = replaceSpace $ fc ^. fcLocation . locCity ++ show (fc ^. fcDate)

-- Initially the application only supported Australia, so city in the topic was
-- sufficient. Have to maintain for backwards compatibility
forecastTopics :: Forecast -> Topics
forecastTopics forecast = Topics $ [locTopic] ++ [legacyTopic | country == "Australia"]
    where legacyTopic = replaceSpace city
          locTopic = replaceSpace $ intercalate "-" [ "v2"
                                                    , country
                                                    , region
                                                    , city
                                                    ]
          loc = forecast ^. fcLocation
          country = loc ^. locCountry
          region = loc ^. locRegion
          city = loc ^. locCity

replaceSpace :: String -> String
replaceSpace = replace " " "_"

showTime :: TimeOfDay -> String
showTime = take 5 . show
