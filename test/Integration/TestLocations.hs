{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Integration.TestLocations where

import Control.Lens
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import Data.Aeson hiding ((.=))
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Set as S
import qualified Data.Vector as V

import App
import Types
import Types.Location

import Integration.Base

import Test.Framework

-- Control.Lens and Aeson clash on .=
aeq :: T.Text -> T.Text -> (T.Text, Value)
aeq = (A..=)

array = Array . V.fromList

test_locations = withApp $ do
    let dummyForecast loc = Forecast { _fcLocation = loc
                                     , _fcDate = fromJust $ fromGregorianValid 2016 2 1
                                     , _fcAlertStart = undefined
                                     , _fcAlertEnd = undefined
                                     , _fcMaxLevel = UVLevel 10
                                     , _fcUpdated = undefined
                                     }
    let locations = [ Location "Australia" "Victoria" "Melbourne"
                    , Location "Australia" "New South Wales" "Sydney"
                    ]
    stateM $ stForecasts .= (S.fromList $ map dummyForecast locations)
    locJson <- getJson "/locations"
    liftIO $ assertEqual (array [ object [ "city" `aeq` "Melbourne" ]
                                , object [ "city" `aeq` "Sydney" ]
                                ]) locJson
