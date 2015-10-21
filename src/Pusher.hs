{-# Language RecordWildCards #-}
module Pusher where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either

import qualified Data.Aeson as A
import Data.List.Utils
import Data.Time.LocalTime

import App
import Data
import Pebble.Client
import Pebble.Types


push :: AppM ()
push = do
    logStr "Pushing"
    forecasts <- stateM $ gets forecasts
    mapM_ putForecastPin forecasts
    logStr "All pushed"

putForecastPin :: Forecast -> AppM ()
putForecastPin forecast = do
    apiKey <- asks coApiKey
    let topics = forecastTopics forecast
    let pin = forecastPin forecast
    result <- liftIO $ runEitherT $ putSharedPin (Just apiKey) (Just topics) (pinId pin) pin
    case result of
        Left err -> error $ show err
        Right result' -> return ()

forecastPin :: Forecast -> Pin
forecastPin forecast@Forecast{..} = Pin { pinId = pinId
                                        , pinTime = fcStartTimeUtc forecast
                                        , pinDuration = Just $ fcDuration forecast
                                        , pinCreateNotification = Just createNotification
                                        , pinUpdateNotification = Just updateNotification
                                        , pinLayout = layout
                                        , pinReminders = []
                                        , pinActions = []
                                        }
    where layout = Layout { layoutType = WeatherPin
                          , layoutTitle = "UV level: " ++ show (uvValue maxLevel)
                          , layoutSubtitle = Just $ "From: " ++ showTime alertStart ++ " To: " ++ showTime alertEnd
                          , layoutBody = Nothing
                          , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                          , layoutSmallIcon = Nothing
                          , layoutLargeIcon = Nothing
                          , layoutPrimaryColor = Nothing
                          , layoutSecondaryColor = Nothing
                          , layoutBackgroundColor = Nothing
                          , layoutParagraphs = []
                          , layoutLastUpdated = Nothing
                          }
          createNotification = Notification { notificationLayout = layout
                                            , notificationTime = Nothing
                                            }
          updateNotification = Notification { notificationLayout = layout
                                            , notificationTime = Just $ fcStartTimeUtc forecast
                                            }
          pinId = replaceSpace $ city location ++ show date

forecastTopics :: Forecast -> Topics
forecastTopics forecast = Topics [locTopic]
    where locTopic = replaceSpace $ city $ location forecast

replaceSpace = replace " " "_"

showTime :: TimeOfDay -> String
showTime = take 5 . show
