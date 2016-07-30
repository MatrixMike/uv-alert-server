{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Pebble.Types where

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Vector as V

import Servant


data Pin = Pin { pinId :: T.Text
               , pinTime :: UTCTime
               , pinDuration :: Maybe Int  -- minutes
               , pinCreateNotification :: Maybe Notification
               , pinUpdateNotification :: Maybe Notification
               , pinLayout :: Layout
               , pinReminders :: [Reminder]
               , pinActions :: [Action]
               }

maybePair :: ToJSON a => T.Text -> Maybe a -> [Pair]
maybePair _ Nothing = []
maybePair name (Just value) = [name .= value]

instance ToJSON Pin where
    toJSON Pin{..} = object $ concat [
            ["id" .= pinId],
            ["time" .= pinTime],
            maybePair "duration" pinDuration,
            maybePair "createNotification" pinCreateNotification,
            maybePair "updateNotification" pinUpdateNotification,
            ["layout" .= pinLayout]
        ]


data Notification = Notification { notificationLayout :: Layout
                                 , notificationTime :: Maybe UTCTime
                                 }

instance ToJSON Notification where
    toJSON Notification{..} = object $
            ("layout" .= notificationLayout) :
            maybePair "time" notificationTime

data Layout = Layout { layoutType :: PinType
                     , layoutTitle :: String
                     , layoutSubtitle :: Maybe String
                     , layoutBody :: Maybe String
                     -- TODO: separate type for an icon?
                     , layoutTinyIcon :: Maybe String
                     , layoutSmallIcon :: Maybe String
                     , layoutLargeIcon :: Maybe String
                     , layoutPrimaryColor :: Maybe Color
                     , layoutSecondaryColor :: Maybe Color
                     , layoutBackgroundColor :: Maybe Color
                     , layoutParagraphs :: [Paragraph]
                     , layoutLastUpdated :: Maybe UTCTime
                     }

instance ToJSON Layout where
    toJSON Layout{..} = object $ concat [
            pinTypeJSON layoutType,
            ["title" .= layoutTitle],
            maybePair "subtitle" layoutSubtitle,
            maybePair "body" layoutBody,
            maybePair "tinyIcon" layoutTinyIcon,
            maybePair "smallIcon" layoutSmallIcon,
            maybePair "largeIcon" layoutLargeIcon,
            maybePair "primaryColor" layoutPrimaryColor,
            maybePair "secondaryColor" layoutSecondaryColor,
            maybePair "backgroundColor" layoutBackgroundColor,
            maybePair "headings" paragraphHeadings,
            maybePair "paragraphs" paragraphTexts
        ]
        where paragraphHeadings = liftM (map paragraphHeading) maybeParagraphs
              paragraphTexts = liftM (map paragraphText) maybeParagraphs
              maybeParagraphs = case layoutParagraphs of
                  [] -> Nothing
                  paragraphs -> Just paragraphs

data PinType = GenericPin
             | CalendarPin  -- TODO: locationName
             | SportsPin  -- TODO: special fields for sports
             | WeatherPin { layoutLocationName :: String }
             | GenericReminder
             | GenericNotification

pinTypeJSON :: PinType -> [Pair]
pinTypeJSON GenericPin = ["type" .= ("genericPin" :: String)]
pinTypeJSON CalendarPin = ["type" .= ("calendarPin" :: String)]
pinTypeJSON SportsPin = ["type" .= ("sportsPin" :: String)]
pinTypeJSON WeatherPin{..} = [
        "type" .= ("weatherPin" :: String),
        "locationName" .= layoutLocationName
    ]
pinTypeJSON GenericReminder = ["type" .= ("genericReminder" :: String)]
pinTypeJSON GenericNotification = ["type" .= ("genericNotification" :: String)]

data Color = Color Int  -- Hex

instance ToJSON Color where
    toJSON = toJSON . hexColor

-- TODO: this must be already defined somewhere
hexColor :: Color -> String
hexColor (Color clr) = '#':hexStr clr
    where hexStr c | c < 16 = [hexChr c]
                   | otherwise = hexStr (c `div` 16) ++ [hexChr (c `mod` 16)]
          hexChr :: Int -> Char
          hexChr i = "0123456789abcdef" !! i

data Paragraph = Paragraph { paragraphHeading :: String
                           , paragraphText :: String
                           }

data Reminder = Reminder { reminderTime :: UTCTime
                         , reminderLayout :: Layout
                         }

data Action = Action { actionTitle :: String
                     , actionType :: ActionType
                     , launchCode :: Int
                     -- TODO: HTTP action details
                     }

data ActionType = OpenWatchAppAction
                | HttpAction

data UserToken = UserToken String

instance ToHttpApiData UserToken where
    toUrlPiece (UserToken token) = T.pack token

data APIKey = APIKey String

instance ToHttpApiData APIKey where
    toUrlPiece (APIKey key) = T.pack key

data Topics = Topics [T.Text]

instance ToHttpApiData Topics where
    toUrlPiece (Topics topics) = T.intercalate "," topics

instance FromJSON Topics where
    parseJSON (Object v) = do
        topicArray <- v .: "topics"
        case topicArray of
            Array topics -> do
                topicStrings <- V.mapM parseJSON topics
                return $ Topics $ V.toList topicStrings
            _ -> mzero
    parseJSON _ = mzero
