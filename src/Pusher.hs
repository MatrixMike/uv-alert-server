{-# Language RecordWildCards #-}
module Pusher where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either

import App
import Data
import Pebble.Client
import Pebble.Types


push :: AppM ()
push = do
    forecasts <- stateM $ gets forecasts
    mapM_ putForecastPin forecasts

putForecastPin :: Forecast -> AppM ()
putForecastPin forecast = do
    apiKey <- asks coApiKey
    let topics = Topics [city $ location forecast]
    let pin = forecastPin forecast
    result <- liftIO $ runEitherT $ putSharedPin (Just apiKey) (Just topics) (pinId pin) pin
    case result of
        Left err -> error $ show err
        Right result' -> return result'

forecastPin :: Forecast -> Pin
forecastPin forecast@Forecast{..} = Pin { pinId = city location ++ show date
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
