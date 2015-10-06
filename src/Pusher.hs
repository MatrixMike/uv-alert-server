{-# Language RecordWildCards #-}
module Pusher where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either

import qualified Data.Aeson as A
import Data.List.Utils

import App
import Data
import Pebble.Client
import Pebble.Types


push :: AppM ()
push = do
    liftIO $ putStrLn "Pushing"
    forecasts <- stateM $ gets forecasts
    mapM_ putForecastPin forecasts

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
                                        , pinCreateNotification = Nothing
                                        , pinUpdateNotification = Nothing
                                        , pinLayout = layout
                                        , pinReminders = []
                                        , pinActions = []
                                        }
    where layout = Layout { layoutType = WeatherPin
                          , layoutTitle = "UV Alert"
                          , layoutSubtitle = Nothing
                          , layoutBody = Nothing
                          , layoutTinyIcon = Nothing
                          , layoutSmallIcon = Nothing
                          , layoutLargeIcon = Nothing
                          , layoutPrimaryColor = Nothing
                          , layoutSecondaryColor = Nothing
                          , layoutBackgroundColor = Nothing
                          , layoutParagraphs = []
                          , layoutLastUpdated = Nothing
                          }
          pinId = replaceSpace $ city location ++ show date

forecastTopics :: Forecast -> Topics
forecastTopics forecast = Topics [locTopic]
    where locTopic = replaceSpace $ city $ location forecast

replaceSpace = replace " " "_"
