{-# LANGUAGE TemplateHaskell #-}
module Types.Location.USA (
    usTZ,
) where

import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson.TH


central :: TimeZoneSeries
central = $(loadTZFile "/usr/share/zoneinfo/US/Central")

pacific :: TimeZoneSeries
pacific = $(loadTZFile "/usr/share/zoneinfo/US/Pacific")

eastern :: TimeZoneSeries
eastern = $(loadTZFile "/usr/share/zoneinfo/US/Eastern")

mountain :: TimeZoneSeries
mountain = $(loadTZFile "/usr/share/zoneinfo/US/Mountain")

alaska :: TimeZoneSeries
alaska = $(loadTZFile "/usr/share/zoneinfo/US/Alaska")

hawaii :: TimeZoneSeries
hawaii = $(loadTZFile "/usr/share/zoneinfo/US/Hawaii")

usTZ :: String -> String -> TimeZoneSeries
usTZ "Elk Grove" "California" = pacific
usTZ "Bellevue" "Washington" = pacific
usTZ "Arlington" "Texas" = central
usTZ "Clarksville" "Tennessee" = central
usTZ "Albuquerque" "New Mexico" = mountain
usTZ "Augusta" "Georgia" = eastern
usTZ "Bakersfield" "California" = pacific
usTZ "Alexandria" "Virginia" = eastern
usTZ "Charleston" "South Carolina" = eastern
usTZ "Cary" "North Carolina" = eastern
usTZ "Akron" "Ohio" = eastern
usTZ "Durham" "North Carolina" = eastern
usTZ "Fayetteville" "North Carolina" = eastern
usTZ "Anchorage" "Alaska" = alaska
usTZ "Columbus" "Georgia" = eastern
usTZ "Birmingham" "Alabama" = central
usTZ "Des Moines" "Iowa" = central
usTZ "Chula Vista" "California" = pacific
usTZ "Bridgeport" "Connecticut" = eastern
usTZ "Colorado Springs" "Colorado" = mountain
usTZ "Amarillo" "Texas" = central
usTZ "Corona" "California" = pacific
usTZ "Anaheim" "California" = pacific
usTZ "El Paso" "Texas" = mountain
usTZ "Austin" "Texas" = central
usTZ "Cape Coral" "Florida" = eastern
usTZ "Chandler" "Arizona" = mountain
usTZ "Chesapeake" "Virginia" = eastern
usTZ "Boise" "Idaho" = mountain
usTZ "Columbia" "South Carolina" = eastern
usTZ "Baton Rouge" "Louisiana" = central
usTZ "Buffalo" "New York" = eastern
usTZ "Eugene" "Oregon" = pacific
usTZ "Brownsville" "Texas" = central
usTZ "Escondido" "California" = pacific
usTZ "Fontana" "California" = pacific
usTZ "Charlotte" "North Carolina" = eastern
usTZ "Chattanooga" "Tennessee" = eastern
usTZ "Dayton" "Ohio" = eastern
usTZ "Corpus Christi" "Texas" = central
usTZ "Aurora" "Illinois" = central
usTZ "Aurora" "Colorado" = mountain
usTZ "Fullerton" "California" = pacific
usTZ "Huntsville" "Alabama" = central
usTZ "Fort Wayne" "Indiana" = eastern
usTZ "Long Beach" "California" = pacific
usTZ "Garland" "Texas" = central
usTZ "Lakewood" "Colorado" = mountain
usTZ "Garden Grove" "California" = pacific
usTZ "Macon" "Georgia" = eastern
usTZ "Hampton" "Virginia" = eastern
usTZ "McKinney" "Texas" = central
usTZ "Irving" "Texas" = central
usTZ "Joliet" "Illinois" = central
usTZ "Frisco" "Texas" = central
usTZ "Jersey City" "New Jersey" = eastern
usTZ "Kansas City" "Missouri" = central
usTZ "Kansas City" "Kansas" = central
usTZ "Hialeah" "Florida" = eastern
usTZ "Fremont" "California" = pacific
usTZ "Louisville" "Kentucky" = eastern
usTZ "Fresno" "California" = pacific
usTZ "Hollywood" "Florida" = eastern
usTZ "Grand Prairie" "Texas" = central
usTZ "Killeen" "Texas" = central
usTZ "Greensboro" "North Carolina" = eastern
usTZ "Jacksonville" "Florida" = eastern
usTZ "Laredo" "Texas" = central
usTZ "Glendale" "California" = pacific
usTZ "Glendale" "Arizona" = mountain
usTZ "Jackson" "Mississippi" = central
usTZ "Grand Rapids" "Michigan" = eastern
usTZ "Irvine" "California" = pacific
usTZ "Lubbock" "Texas" = central
usTZ "Knoxville" "Tennessee" = eastern
usTZ "Fort Lauderdale" "Florida" = eastern
usTZ "Henderson" "Nevada" = pacific
usTZ "McAllen" "Texas" = central
usTZ "Lincoln" "Nebraska" = central
usTZ "Huntington Beach" "California" = pacific
usTZ "Little Rock" "Arkansas" = central
usTZ "Fort Worth" "Texas" = central
usTZ "Fort Collins" "Colorado" = mountain
usTZ "Gilbert" "Arizona" = mountain
usTZ "Memphis" "Tennessee" = central
usTZ "Lancaster" "California" = pacific
usTZ "Madison" "Wisconsin" = central
usTZ "Hayward" "California" = pacific
usTZ "Lexington" "Kentucky" = eastern
usTZ "Sacramento" "California" = pacific
usTZ "Mobile" "Alabama" = central
usTZ "Providence" "Rhode Island" = eastern
usTZ "Overland Park" "Kansas" = central
usTZ "Oakland" "California" = pacific
usTZ "Pasadena" "California" = pacific
usTZ "Pasadena" "Texas" = central
usTZ "Norfolk" "Virginia" = eastern
usTZ "Reno" "Nevada" = pacific
usTZ "Rockford" "Illinois" = central
usTZ "Rancho Cucamonga" "California" = pacific
usTZ "Orlando" "Florida" = eastern
usTZ "Nashville" "Tennessee" = central
usTZ "Moreno Valley" "California" = pacific
usTZ "Phoenix" "Arizona" = mountain
usTZ "Riverside" "California" = pacific
usTZ "Miramar" "Florida" = eastern
usTZ "Peoria" "Arizona" = mountain
usTZ "Oxnard" "California" = pacific
usTZ "Newport News" "Virginia" = eastern
usTZ "Montgomery" "Alabama" = central
usTZ "Palmdale" "California" = pacific
usTZ "Ontario" "California" = pacific
usTZ "Richmond" "Virginia" = eastern
usTZ "Orange" "California" = pacific
usTZ "Mesa" "Arizona" = mountain
usTZ "Newark" "New Jersey" = eastern
usTZ "Pomona" "California" = pacific
usTZ "Raleigh" "North Carolina" = eastern
usTZ "North Las Vegas" "Nevada" = pacific
usTZ "Omaha" "Nebraska" = central
usTZ "Pembroke Pines" "Florida" = eastern
usTZ "Olathe" "Kansas" = central
usTZ "Paterson" "New Jersey" = eastern
usTZ "Port St. Lucie" "Florida" = eastern
usTZ "Portland" "Oregon" = pacific
usTZ "Modesto" "California" = pacific
usTZ "Oceanside" "California" = pacific
usTZ "Plano" "Texas" = central
usTZ "New Haven" "Connecticut" = eastern
usTZ "Rochester" "New York" = eastern
usTZ "Mesquite" "Texas" = central
usTZ "Shreveport" "Louisiana" = central
usTZ "Springfield" "Massachusetts" = eastern
usTZ "Springfield" "Missouri" = central
usTZ "Virginia Beach" "Virginia" = eastern
usTZ "Saint Paul" "Minnesota" = central
usTZ "Wichita" "Kansas" = central
usTZ "Santa Rosa" "California" = pacific
usTZ "Tallahassee" "Florida" = eastern
usTZ "Sioux Falls" "South Dakota" = central
usTZ "Scottsdale" "Arizona" = mountain
usTZ "Santa Ana" "California" = pacific
usTZ "Worcester" "Massachusetts" = eastern
usTZ "Yonkers" "New York" = eastern
usTZ "Toledo" "Ohio" = eastern
usTZ "San Bernardino" "California" = pacific
usTZ "Tempe" "Arizona" = mountain
usTZ "Torrance" "California" = pacific
usTZ "Tulsa" "Oklahoma" = central
usTZ "Savannah" "Georgia" = eastern
usTZ "Stockton" "California" = pacific
usTZ "Tacoma" "Washington" = pacific
usTZ "Vancouver" "Washington" = pacific
usTZ "San Jose" "California" = pacific
usTZ "St. Petersburg" "Florida" = eastern
usTZ "Spokane" "Washington" = pacific
usTZ "Sunnyvale" "California" = pacific
usTZ "Tucson" "Arizona" = mountain
usTZ "Santa Clarita" "California" = pacific
usTZ "Salinas" "California" = pacific
usTZ "Syracuse" "New York" = eastern
usTZ "Tampa" "Florida" = eastern
usTZ "West Valley City" "Utah" = mountain
usTZ "Warren" "Michigan" = eastern
usTZ "Thornton" "Colorado" = mountain
usTZ "Waco" "Texas" = central
usTZ "Sterling Heights" "Michigan" = eastern
usTZ "Salem" "Oregon" = pacific
usTZ "Washington" "District of Columbia" = eastern
usTZ "Honolulu" "Hawaii" = hawaii
usTZ "Boston" "Massachusetts" = eastern
usTZ "San Francisco" "California" = pacific
usTZ "Philadelphia" "Pennsylvania" = eastern
usTZ "Atlanta" "Georgia" = eastern
usTZ "San Antonio" "Texas" = central
usTZ "Oklahoma City" "Oklahoma" = central
usTZ "Cincinnati" "Ohio" = eastern
usTZ "Winston-Salem" "North Carolina" = eastern
usTZ "Los Angeles" "California" = pacific
usTZ "Milwaukee" "Wisconsin" = central
usTZ "Pittsburgh" "Pennsylvania" = eastern
usTZ "Minneapolis" "Minnesota" = central
usTZ "Chicago" "Illinois" = central
usTZ "Salt Lake City" "Utah" = mountain
usTZ "Cleveland" "Ohio" = eastern
usTZ "Miami" "Florida" = eastern
usTZ "Indianapolis" "Indiana" = eastern
usTZ "Houston" "Texas" = central
usTZ "Denver" "Colorado" = mountain
usTZ "New York" "New York" = eastern
usTZ "New Orleans" "Louisiana" = central
usTZ "Seattle" "Washington" = pacific
usTZ "Las Vegas" "Nevada" = pacific
usTZ "St. Louis" "Missouri" = central
usTZ "San Diego" "California" = pacific
usTZ "Detroit" "Michigan" = eastern
usTZ "Dallas" "Texas" = central
usTZ "Baltimore" "Maryland" = eastern
usTZ "Columbus" "Ohio" = eastern
usTZ city state = error $ "Unknown city " ++ city ++ ", " ++ state
