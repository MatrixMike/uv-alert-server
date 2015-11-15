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
    forecasts <- stateM $ gets forecasts
    let fcount = length forecasts
    logStr $ "Pushing " ++ show fcount ++ " forecasts..."
    mapM_ putForecastPin forecasts
    logStr "All pushed."

putForecastPin :: Forecast -> AppM ()
putForecastPin forecast = do
    apiKey <- asks coApiKey
    let topics = forecastTopics forecast
    let pins = forecastPin forecast
    forM_ pins $ \pin -> do
        result <- liftIO $ runEitherT $ putSharedPin (Just apiKey) (Just topics) (pinId pin) pin
        case result of
            Left err -> error $ show err
            Right result' -> return ()

forecastPin :: Forecast -> [Pin]
forecastPin forecast@Forecast{..} = [ Pin { pinId = pinId ++ "start"
                                          , pinTime = fcStartTimeUtc forecast
                                          , pinDuration = Nothing
                                          , pinCreateNotification = Nothing
                                          , pinUpdateNotification = Nothing
                                          , pinLayout = startLayout
                                          , pinReminders = []
                                          , pinActions = []
                                          }
                                    , Pin { pinId = pinId ++ "end"
                                          , pinTime = fcEndTimeUtc forecast
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
                            , layoutSubtitle = Just $ "Max level: " ++ show (uvValue maxLevel)
                            , layoutBody = Just $ "Alert from " ++ showTime alertStart ++ " to " ++ showTime alertEnd
                            , layoutTinyIcon = Nothing
                            , layoutSmallIcon = Nothing
                            , layoutLargeIcon = Nothing
                            , layoutPrimaryColor = Nothing
                            , layoutSecondaryColor = Nothing
                            , layoutBackgroundColor = Nothing
                            , layoutParagraphs = []
                            , layoutLastUpdated = Nothing
                            }
        startLayout = baseLayout { layoutTitle = "UV Alert start"
                                 , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                                 }
        endLayout = baseLayout { layoutTitle = "UV Alert end"
                               , layoutTinyIcon = Just "system://images/TIMELINE_SUN"
                               }
        pinId = replaceSpace $ city location ++ show date

forecastTopics :: Forecast -> Topics
forecastTopics forecast = Topics [locTopic]
    where locTopic = replaceSpace $ city $ location forecast

replaceSpace = replace " " "_"

showTime :: TimeOfDay -> String
showTime = take 5 . show
