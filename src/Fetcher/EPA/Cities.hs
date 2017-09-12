{-# LANGUAGE NegativeLiterals #-}

module Fetcher.EPA.Cities
  ( cities
  , usStateAbbreviation
  ) where

import qualified Data.Map as M
import Data.Maybe

import Types.Location
import Types.Location.TimeZones

cities :: [LocationCoordinates]
cities =
  [ loc "New York" "New York" (latlon 40.67 -73.94) usEastern
  , loc "California" "Los Angeles" (latlon 34.05 -118.25) usPacific
  , loc "Illinois" "Chicago" (latlon 41.9 -87.65) usCentral
  , loc "Texas" "Houston" (latlon 29.762777 -95.38306) usCentral
  , loc "Arizona" "Phoenix" (latlon 33.52833 -112.076385) usMountain
  , loc "Texas" "San Antonio" (latlon 29.428612 -98.49333) usCentral
  , loc "Texas" "Dallas" (latlon 32.7825 -96.7975) usCentral
  , loc "California" "San Jose" (latlon 37.304165 -121.87278) usPacific
  , loc "Texas" "Austin" (latlon 30.3 -97.73333) usCentral
  , loc "Indiana" "Indianapolis" (latlon 39.767776 -86.15806) usEastern
  , loc "Florida" "Jacksonville" (latlon 30.316668 -81.65) usEastern
  , loc "Texas" "Fort Worth" (latlon 32.75722 -97.33305) usCentral
  , loc "California" "San Francisco" (latlon 37.766666 -122.433334) usPacific
  , loc "Ohio" "Columbus" (latlon 39.983334 -82.98333) usEastern
  , loc "Kentucky" "Louisville" (latlon 38.254166 -85.76028) usEastern
  , loc "Michigan" "Detroit" (latlon 42.331665 -83.0475) usEastern
  , loc "Tennessee" "Nashville" (latlon 36.165 -86.78389) usCentral
  , loc "Massachusetts" "Boston" (latlon 42.357777 -71.06167) usEastern
  , loc "Colorado" "Denver" (latlon 39.739166 -104.984726) usMountain
  , loc "Texas" "El Paso" (latlon 31.790277 -106.42333) usMountain
  , loc "Tennessee" "Memphis" (latlon 35.1175 -89.97111) usCentral
  , loc "Maryland" "Baltimore" (latlon 39.28639 -76.615) usEastern
  , loc "Washington" "Seattle" (latlon 47.6 -122.316666) usPacific
  , loc "Oklahoma" "Oklahoma" (latlon 35.4823 -97.5352) usCentral
  , loc "Oregon" "Portland" (latlon 45.516666 -122.666664) usPacific
  , loc "New Mexico" "Albuquerque" (latlon 35.116665 -106.61667) usMountain
  , loc "California" "Fresno" (latlon 36.781666 -119.79222) usPacific
  , loc "Missouri" "Kansas" (latlon 39.05 -94.583336) usCentral
  , loc "Georgia" "Atlanta" (latlon 33.756943 -84.390274) usEastern
  , loc "North Carolina" "Raleigh" (latlon 35.81889 -78.64472) usEastern
  , loc "Arizona" "Mesa" (latlon 33.415 -111.83139) usMountain
  , loc "Colorado" "Colorado Springs" (latlon 38.863335 -104.79195) usMountain
  , loc "Ohio" "Cleveland" (latlon 41.482224 -81.66972) usEastern
  , loc "Oklahoma" "Tulsa" (latlon 36.13139 -95.937225) usCentral
  , loc "Kansas" "Wichita" (latlon 37.68889 -97.33611) usCentral
  , loc "Louisiana" "New Orleans" (latlon 29.966667 -90.05) usCentral
  , loc "Texas" "Arlington" (latlon 32.705032 -97.12284) usCentral
  , loc "Florida" "Tampa" (latlon 27.970833 -82.46472) usEastern
  , loc "California" "Bakersfield" (latlon 35.405834 -119.01861) usPacific
  , loc "California" "Anaheim" (latlon 33.836113 -117.889725) usPacific
  , loc "Colorado" "Aurora" (latlon 39.695835 -104.80805) usMountain
  , loc "California" "Santa Ana" (latlon 33.740833 -117.881386) usPacific
  , loc "Missouri" "St. Louis" (latlon 38.616665 -90.2) usCentral
  , loc "Texas" "Corpus Christi" (latlon 27.742777 -97.40195) usCentral
  , loc "California" "Riverside" (latlon 33.948063 -117.396126) usPacific
  , loc "Alaska" "Anchorage" (latlon 61.218334 -149.89917) usAlaska
  , loc "Kentucky" "Lexington" (latlon 38.029724 -84.49472) usEastern
  , loc "California" "Stockton" (latlon 37.975555 -121.300835) usPacific
  , loc "Minnesota" "Saint Paul" (latlon 44.944168 -93.09361) usCentral
  , loc "New Jersey" "Newark" (latlon 40.73528 -74.185) usEastern
  , loc "Nebraska" "Lincoln" (latlon 40.810555 -96.680275) usCentral
  , loc "North Carolina" "Greensboro" (latlon 36.08 -79.81944) usEastern
  , loc "Texas" "Plano" (latlon 33.05 -96.75) usCentral
  , loc "Nevada" "Henderson" (latlon 36.0292 -115.0253) usPacific
  , loc "Indiana" "Fort Wayne" (latlon 41.08045 -85.13915) usEastern
  , loc "California" "Irvine" (latlon 33.684166 -117.7925) usPacific
  , loc "New Jersey" "Jersey" (latlon 40.711388 -74.06472) usEastern
  , loc "Texas" "Laredo" (latlon 27.506111 -99.507225) usCentral
  , loc "California" "Chula Vista" (latlon 32.627777 -117.04806) usPacific
  , loc "Texas" "Lubbock" (latlon 33.566666 -101.88333) usCentral
  , loc "Florida" "Orlando" (latlon 28.533611 -81.386665) usEastern
  , loc "Wisconsin" "Madison" (latlon 43.074722 -89.384445) usCentral
  , loc "North Carolina" "Winston-Salem" (latlon 36.1025 -80.26056) usEastern
  , loc "Arizona" "Chandler" (latlon 33.303333 -111.84139) usMountain
  , loc "Louisiana" "Baton Rouge" (latlon 30.416666 -91.1) usCentral
  , loc "North Carolina" "Durham" (latlon 35.9875 -78.90722) usEastern
  , loc "Texas" "Garland" (latlon 32.906944 -96.635) usCentral
  , loc "Arizona" "Glendale" (latlon 33.583332 -112.2) usMountain
  , loc "Nevada" "Reno" (latlon 39.52722 -119.821945) usPacific
  , loc "Florida" "Hialeah" (latlon 25.860556 -80.29389) usEastern
  , loc "California" "Fremont" (latlon 37.543056 -121.98278) usPacific
  , loc "California" "Santa Clarita" (latlon 34.41639 -118.506386) usPacific
  , loc "Arizona" "Scottsdale" (latlon 33.493057 -111.92611) usMountain
  , loc "Nevada" "North Las Vegas" (latlon 36.22861 -115.14667) usPacific
  , loc "Texas" "Irving" (latlon 32.81167 -96.950836) usCentral
  , loc "New York" "Rochester" (latlon 43.165554 -77.61139) usEastern
  , loc "Alabama" "Montgomery" (latlon 32.361668 -86.27917) usCentral
  , loc "Idaho" "Boise" (latlon 43.61361 -116.23778) usMountain
  , loc "California" "Oxnard" (latlon 34.191387 -119.1825) usPacific
  , loc "North Carolina" "Fayetteville" (latlon 35.066666 -78.9175) usEastern
  , loc "Iowa" "Des Moines" (latlon 41.59083 -93.620834) usCentral
  , loc "California" "Fontana" (latlon 34.1 -117.46667) usPacific
  , loc "California" "Modesto" (latlon 37.66139 -120.994446) usPacific
  , loc "Georgia" "Columbus" (latlon 32.49222 -84.94028) usEastern
  , loc "Louisiana" "Shreveport" (latlon 32.50821 -93.762955) usCentral
  , loc "Washington" "Tacoma" (latlon 47.24139 -122.45944) usPacific
  , loc "Illinois" "Aurora" (latlon 41.76 -88.298615) usCentral
  , loc "Georgia" "Augusta" (latlon 33.47 -81.975) usEastern
  , loc "Alabama" "Mobile" (latlon 30.727669 -88.05267) usCentral
  , loc "California" "Moreno Valley" (latlon 33.943054 -117.22833) usPacific
  , loc "California" "Glendale" (latlon 34.170834 -118.25) usPacific
  , loc "Texas" "Amarillo" (latlon 35.199165 -101.845276) usCentral
  , loc "California" "Huntington Beach" (latlon 33.69278 -117.999725) usPacific
  , loc "Utah" "Salt Lake" (latlon 40.75 -111.88333) usMountain
  , loc "Texas" "Brownsville" (latlon 25.930277 -97.48444) usCentral
  , loc "Massachusetts" "Worcester" (latlon 42.266666 -71.8) usEastern
  , loc "Florida" "Tallahassee" (latlon 30.438736 -84.28063) usEastern
  , loc "Alabama" "Huntsville" (latlon 34.71361 -86.58611) usCentral
  , loc "Tennessee" "Knoxville" (latlon 35.966667 -83.95) usEastern
  , loc "Texas" "Grand Prairie" (latlon 32.71528 -97.016945) usCentral
  , loc "California" "Oceanside" (latlon 33.211666 -117.325836) usPacific
  , loc "Mississippi" "Jackson" (latlon 32.29889 -90.18472) usCentral
  , loc "Kansas" "Overland Park" (latlon 38.94007 -94.680695) usCentral
  , loc "Tennessee" "Chattanooga" (latlon 35.045555 -85.26722) usEastern
  , loc "Florida" "Fort Lauderdale" (latlon 26.135834 -80.141945) usEastern
  , loc "California" "Garden Grove" (latlon 33.77889 -117.96028) usPacific
  , loc "California" "Santa Rosa" (latlon 38.448612 -122.70472) usPacific
  , loc
      "California"
      "Rancho Cucamonga"
      (latlon 34.123333 -117.579445)
      usPacific
  , loc "South Dakota" "Sioux Falls" (latlon 43.53639 -96.73167) usCentral
  , loc "Florida" "Port St. Lucie" (latlon 27.275833 -80.355) usEastern
  , loc "Washington" "Vancouver" (latlon 45.63361 -122.602776) usPacific
  , loc "Arizona" "Tempe" (latlon 33.429443 -111.943054) usMountain
  , loc "California" "Corona" (latlon 33.866665 -117.566666) usPacific
  , loc "Missouri" "Springfield" (latlon 37.195 -93.28611) usCentral
  , loc "California" "Lancaster" (latlon 34.686943 -118.15417) usPacific
  , loc "Texas" "McKinney" (latlon 33.2 -96.63333) usCentral
  , loc "Florida" "Pembroke Pines" (latlon 26.0125 -80.313614) usEastern
  , loc "Oregon" "Salem" (latlon 44.930832 -123.02889) usPacific
  , loc "Florida" "Cape Coral" (latlon 26.633333 -81.98333) usEastern
  , loc "Arizona" "Peoria" (latlon 33.5825 -112.23861) usMountain
  , loc "Georgia" "Macon" (latlon 32.83583 -83.64639) usEastern
  , loc "Massachusetts" "Springfield" (latlon 42.11241 -72.547455) usEastern
  , loc "California" "Elk Grove" (latlon 38.43833 -121.38194) usPacific
  , loc "California" "Palmdale" (latlon 34.581112 -118.100555) usPacific
  , loc "California" "Salinas" (latlon 36.677776 -121.655556) usPacific
  , loc "California" "Hayward" (latlon 37.668888 -122.08083) usPacific
  , loc "California" "Pomona" (latlon 34.060833 -117.75584) usPacific
  , loc "Texas" "Pasadena" (latlon 29.676111 -95.17389) usCentral
  , loc "Virginia" "Alexandria" (latlon 38.80472 -77.047226) usEastern
  , loc
      "New Jersey"
      "Paterson, New Jersey"
      (latlon 40.915554 -74.163055)
      usEastern
  , loc "Kansas" "Kansas" (latlon 39.106667 -94.67639) usCentral
  , loc "California" "Torrance" (latlon 33.83585 -118.34063) usPacific
  , loc "Colorado" "Fort Collins" (latlon 40.566666 -105.083336) usMountain
  , loc "California" "Escondido" (latlon 33.12472 -117.08083) usPacific
  , loc "Texas" "Mesquite" (latlon 32.78278 -96.60972) usCentral
  , loc "Colorado" "Lakewood" (latlon 39.70639 -105.102776) usMountain
  , loc "Georgia" "Savannah" (latlon 32.050835 -81.10389) usEastern
  , loc "Illinois" "Naperville" (latlon 41.748055 -88.16556) usCentral
  , loc "Ohio" "Dayton" (latlon 39.766666 -84.2) usEastern
  , loc "Texas" "McAllen" (latlon 26.216389 -98.23639) usCentral
  , loc "California" "Sunnyvale" (latlon 37.36889 -122.03694) usPacific
  , loc "Texas" "Huntsville" (latlon 30.723328 -95.55096) usCentral
  , loc "Texas" "Killeen" (latlon 31.105556 -97.72667) usCentral
  , loc "California" "Orange" (latlon 33.803055 -117.8325) usPacific
  , loc "California" "Fullerton" (latlon 33.88 -117.92861) usPacific
  , loc "Michigan" "Warren" (latlon 42.491943 -83.02389) usEastern
  , loc "Tennessee" "Clarksville" (latlon 36.52972 -87.35944) usCentral
  , loc "Utah" "West Valley" (latlon 40.689167 -111.99389) usMountain
  , loc "California" "Visalia" (latlon 36.317223 -119.33195) usPacific
  , loc "Connecticut" "New Haven" (latlon 41.308334 -72.925) usEastern
  , loc "Michigan" "Sterling Heights" (latlon 42.580276 -83.03028) usEastern
  , loc "Florida" "Miramar" (latlon 25.97889 -80.2825) usEastern
  , loc "Florida" "Gainesville" (latlon 29.665277 -82.33611) usEastern
  , loc "Connecticut" "Stamford" (latlon 41.09667 -73.55222) usEastern
  , loc "Kansas" "Topeka" (latlon 39.033333 -95.683334) usCentral
  , loc "Texas" "Carrollton" (latlon 32.99 -96.89333) usCentral
  , loc "California" "Thousand Oaks" (latlon 34.189445 -118.875) usPacific
  , loc "Iowa" "Cedar Rapids" (latlon 41.983334 -91.66861) usCentral
  , loc "New Jersey" "Elizabeth" (latlon 40.662224 -74.20917) usEastern
  , loc "California" "Concord" (latlon 37.978054 -122.03111) usPacific
  , loc "Kansas" "Olathe" (latlon 38.8808 -94.8031) usCentral
  , loc "Connecticut" "Hartford" (latlon 41.763332 -72.685) usEastern
  , loc "Texas" "Waco" (latlon 31.55139 -97.15583) usCentral
  , loc "California" "Simi Valley" (latlon 34.270832 -118.73917) usPacific
  , loc "Texas" "Midland" (latlon 32.005 -102.09917) usCentral
  , loc "Washington" "Bellevue" (latlon 47.5975 -122.159164) usPacific
  , loc "Florida" "Coral Springs" (latlon 26.270555 -80.25916) usEastern
  , loc "Louisiana" "Lafayette" (latlon 30.216667 -92.03333) usCentral
  , loc "South Carolina" "Charleston" (latlon 32.77611 -79.9325) usEastern
  , loc "Florida" "Panama Beach" (latlon 30.176666 -85.80556) usCentral
  , loc "Colorado" "Thornton" (latlon 39.9031 -104.954) usMountain
  , loc "Texas" "Beaumont" (latlon 30.08 -94.12666) usCentral
  , loc "Arizona" "Surprise" (latlon 33.629196 -112.3678) usMountain
  , loc "Indiana" "Evansville" (latlon 37.974724 -87.55583) usCentral
  , loc "Texas" "Abilene" (latlon 32.44639 -99.74555) usCentral
  , loc "Texas" "Frisco" (latlon 33.1414 -96.8131) usCentral
  , loc "Utah" "Provo" (latlon 40.244446 -111.660835) usMountain
  , loc "California" "Vallejo" (latlon 38.104088 -122.25664) usPacific
  , loc "California" "Victorville" (latlon 34.53611 -117.28833) usPacific
  , loc "Illinois" "Peoria" (latlon 40.720833 -89.60944) usCentral
  , loc "Georgia" "Athens" (latlon 33.955276 -83.38306) usEastern
  , loc "Michigan" "Lansing" (latlon 42.7335 -84.5467) usEastern
  , loc "California" "El Monte" (latlon 34.073334 -118.0275) usPacific
  , loc "Texas" "Denton" (latlon 33.216667 -97.13333) usCentral
  , loc "California" "Berkeley" (latlon 37.870277 -122.26806) usPacific
  , loc "Michigan" "Grand Rapids" (latlon 42.96125 -85.655716) usEastern
  , loc "California" "Downey" (latlon 33.938057 -118.13084) usPacific
  , loc "Oklahoma" "Norman" (latlon 35.22167 -97.418335) usCentral
  , loc "Minnesota" "Rochester" (latlon 44.0234 -92.46295) usCentral
  , loc "Texas" "Odessa" (latlon 31.863333 -102.365555) usCentral
  , loc "Connecticut" "Waterbury" (latlon 41.558334 -73.03694) usEastern
  , loc "Illinois" "Elgin" (latlon 42.039444 -88.28861) usCentral
  , loc "California" "Costa Mesa" (latlon 33.67 -117.91) usPacific
  , loc "Massachusetts" "Lowell" (latlon 42.639446 -71.31472) usEastern
  , loc "Texas" "Round Rock" (latlon 30.515 -97.6725) usCentral
  , loc "California" "Inglewood" (latlon 33.9575 -118.34611) usPacific
  , loc "New Hampshire" "Manchester" (latlon 42.990833 -71.46361) usEastern
  , loc "Tennessee" "Murfreesboro" (latlon 35.84614 -86.392075) usCentral
  , loc "Texas" "Pearland" (latlon 29.5544 -95.2958) usCentral
  , loc "Missouri" "Columbia" (latlon 38.948334 -92.333885) usCentral
  , loc "Utah" "West Jordan" (latlon 40.606388 -111.97611) usMountain
  , loc "Florida" "Clearwater" (latlon 27.973612 -82.76417) usEastern
  , loc "Texas" "Tyler" (latlon 32.35 -95.3) usCentral
  , loc "Massachusetts" "Cambridge" (latlon 42.375 -71.10611) usEastern
  , loc "Florida" "Miami Gardens" (latlon 25.942122 -80.26992) usEastern
  , loc "Colorado" "Pueblo" (latlon 38.266945 -104.62028) usMountain
  , loc "Colorado" "Arvada" (latlon 39.802765 -105.08749) usMountain
  , loc "California" "Ventura" (latlon 34.275 -119.227776) usPacific
  , loc "Colorado" "Westminster" (latlon 39.836113 -105.037224) usMountain
  , loc "California" "West Covina" (latlon 34.056667 -117.91861) usPacific
  , loc "California" "Norwalk" (latlon 33.906944 -118.083336) usPacific
  , loc "North Dakota" "Fargo" (latlon 46.877224 -96.789444) usCentral
  , loc "California" "Fairfield" (latlon 38.25778 -122.05417) usPacific
  , loc "California" "Roseville" (latlon 38.7525 -121.289444) usPacific
  , loc "Texas" "Wichita Falls" (latlon 33.896946 -98.515) usCentral
  , loc "North Carolina" "High Point" (latlon 35.970554 -79.9975) usEastern
  , loc "Wisconsin" "Green Bay" (latlon 44.513332 -88.01583) usCentral
  , loc "Florida" "West Palm Beach" (latlon 26.709723 -80.06416) usEastern
  , loc "California" "Richmond" (latlon 37.935833 -122.34778) usPacific
  , loc "California" "Murrieta" (latlon 33.569443 -117.2025) usPacific
  , loc "California" "Burbank" (latlon 34.18028 -118.32833) usPacific
  , loc "Florida" "Palm Bay" (latlon 27.997923 -80.670006) usEastern
  , loc "Washington" "Everett" (latlon 47.963333 -122.200554) usPacific
  , loc "Michigan" "Flint" (latlon 43.01 -83.69) usEastern
  , loc "California" "Antioch" (latlon 38.005 -121.80583) usPacific
  , loc "Georgia" "Sandy Springs" (latlon 33.9375 -84.368614) usEastern
  , loc "Indiana" "South Bend" (latlon 41.6725 -86.25528) usEastern
  , loc "California" "Daly" (latlon 37.68639 -122.46833) usPacific
  , loc "Florida" "Lakeland" (latlon 28.041111 -81.958885) usEastern
  , loc "Colorado" "Centennial" (latlon 39.59639 -104.84389) usMountain
  , loc "New York" "Albany" (latlon 42.65 -73.76667) usEastern
  , loc "California" "Temecula" (latlon 33.503334 -117.12361) usPacific
  , loc "Texas" "College Station" (latlon 30.601389 -96.314445) usCentral
  , loc "Florida" "Pompano Beach" (latlon 26.234722 -80.12556) usEastern
  , loc "California" "Santa Maria" (latlon 34.95139 -120.433334) usPacific
  , loc "California" "El Cajon" (latlon 32.798332 -116.96) usPacific
  , loc "Texas" "Richardson" (latlon 32.965557 -96.715836) usCentral
  , loc "Wisconsin" "Kenosha" (latlon 42.582222 -87.84556) usCentral
  , loc "Texas" "Allen" (latlon 33.099724 -96.663055) usCentral
  , loc "California" "Rialto" (latlon 34.11139 -117.3825) usPacific
  , loc "Oklahoma" "Broken Arrow" (latlon 36.03639 -95.78361) usCentral
  , loc "California" "Jurupa Valley" (latlon 33.9994 -117.475) usPacific
  , loc "Michigan" "Dearborn" (latlon 42.31133 -83.21348) usEastern
  , loc "New Mexico" "Las Cruces" (latlon 32.31972 -106.765274) usMountain
  , loc
      "South Carolina"
      "North Charleston"
      (latlon 32.885277 -80.016945)
      usEastern
  , loc "Colorado" "Boulder" (latlon 40.019444 -105.29278) usMountain
  , loc "California" "San Mateo" (latlon 37.554165 -122.31306) usPacific
  , loc "Michigan" "Livonia" (latlon 42.39722 -83.37361) usEastern
  , loc "Michigan" "Livonia" (latlon 42.40111 -83.37111) usEastern
  , loc "California" "Compton" (latlon 33.896667 -118.225) usPacific
  , loc "California" "Clovis" (latlon 36.82528 -119.70306) usPacific
  , loc "Texas" "Lewisville" (latlon 33.0383 -97.0061) usCentral
  , loc "Massachusetts" "New Bedford" (latlon 41.636112 -70.93472) usEastern
  , loc "Georgia" "Roswell" (latlon 34.0339 -84.3442) usEastern
  , loc "California" "Vista" (latlon 33.19361 -117.24111) usPacific
  , loc "Massachusetts" "Brockton" (latlon 42.083332 -71.01889) usEastern
  , loc "California" "Mission Viejo" (latlon 33.612778 -117.65611) usPacific
  , loc "Texas" "San Angelo" (latlon 31.45 -100.45) usCentral
  , loc "Arizona" "Yuma" (latlon 32.692223 -114.61528) usMountain
  , loc "Colorado" "Greeley" (latlon 40.416668 -104.7) usMountain
  , loc "California" "Vacaville" (latlon 38.35389 -121.97278) usPacific
  , loc "Massachusetts" "Quincy" (latlon 42.25 -71.0) usEastern
  , loc "California" "Carson" (latlon 33.83972 -118.25972) usPacific
  , loc "Oregon" "Hillsboro" (latlon 45.523056 -122.988335) usPacific
  , loc "Missouri" "Lee's Summit" (latlon 38.9225 -94.37417) usCentral
  , loc "Washington" "Yakima" (latlon 46.601944 -120.507774) usPacific
  , loc "Washington" "Renton" (latlon 47.486668 -122.195274) usPacific
  , loc "Utah" "Orem" (latlon 40.29875 -111.69649) usMountain
  , loc "Alabama" "Tuscaloosa" (latlon 33.206665 -87.53472) usCentral
  , loc "Massachusetts" "Lynn" (latlon 42.466667 -70.95) usEastern
  , loc "California" "Hesperia" (latlon 34.412777 -117.306114) usPacific
  , loc "Oregon" "Beaverton" (latlon 45.486946 -122.80361) usPacific
  , loc "Washington" "Spokane Valley" (latlon 47.6733 -117.239) usPacific
  , loc "California" "Santa Monica" (latlon 34.021946 -118.48139) usPacific
  , loc "California" "Westminster" (latlon 33.75139 -117.99389) usPacific
  , loc "Utah" "Sandy" (latlon 40.5725 -111.859726) usMountain
  , loc "Massachusetts" "Fall River" (latlon 41.70139 -71.155556) usEastern
  , loc "Florida" "Sunrise" (latlon 26.157223 -80.28611) usEastern
  , loc "Connecticut" "Norwalk" (latlon 41.093887 -73.41972) usEastern
  , loc "Kansas" "Lawrence" (latlon 38.97167 -95.235275) usCentral
  , loc "New Mexico" "Rio Rancho" (latlon 35.28611 -106.670555) usMountain
  , loc "California" "Livermore" (latlon 37.685555 -121.76417) usPacific
  , loc "New Hampshire" "Nashua" (latlon 42.7575 -71.46445) usEastern
  , loc "Colorado" "Longmont" (latlon 40.171665 -105.10917) usMountain
  , loc "California" "Chico" (latlon 39.74 -121.835556) usPacific
  , loc "Indiana" "Carmel" (latlon 39.966667 -86.1) usEastern
  , loc "California" "Whittier" (latlon 33.965557 -118.024445) usPacific
  , loc "California" "Newport Beach" (latlon 33.616665 -117.8975) usPacific
  , loc "Florida" "Deltona" (latlon 28.905 -81.21111) usEastern
  , loc "California" "San Leandro" (latlon 37.725 -122.15611) usPacific
  , loc "New Jersey" "Trenton" (latlon 40.22167 -74.75611) usEastern
  , loc "Illinois" "Champaign" (latlon 40.112778 -88.26111) usCentral
  , loc "Alabama" "Hoover" (latlon 33.386112 -86.80556) usCentral
  , loc "Michigan" "Westland" (latlon 42.324165 -83.400276) usEastern
  , loc "Texas" "Sugar Land" (latlon 29.599445 -95.61417) usCentral
  , loc "Utah" "Ogden" (latlon 41.22778 -111.96111) usMountain
  , loc "California" "San Marcos" (latlon 33.141945 -117.17028) usPacific
  , loc "Texas" "League" (latlon 29.499722 -95.08972) usCentral
  , loc "Idaho" "Nampa" (latlon 43.574722 -116.563614) usMountain
  , loc "California" "Citrus Heights" (latlon 38.69 -121.29) usPacific
  , loc "California" "Alhambra" (latlon 34.081944 -118.135) usPacific
  , loc "California" "Tracy" (latlon 37.738056 -121.43389) usPacific
  , loc "Minnesota" "Bloomington" (latlon 44.83361 -93.31) usCentral
  , loc "Rhode Island" "Warwick" (latlon 41.716667 -71.416664) usEastern
  , loc "Connecticut" "Danbury" (latlon 41.40222 -73.47111) usEastern
  , loc "Ohio" "Parma" (latlon 41.391666 -81.727776) usEastern
  , loc "Oklahoma" "Edmond" (latlon 35.65 -97.45) usCentral
  , loc "Texas" "Edinburg" (latlon 26.3042 -98.1639) usCentral
  , loc "Michigan" "Troy" (latlon 42.580276 -83.14306) usEastern
  , loc "Washington" "Bellingham" (latlon 48.75028 -122.475) usPacific
  , loc "Indiana" "Hammond" (latlon 41.6111 -87.4931) usCentral
  , loc "California" "Buena Park" (latlon 33.85611 -118.004166) usPacific
  , loc "Texas" "Longview" (latlon 32.5092 -94.7539) usCentral
  , loc "Texas" "Mission" (latlon 26.21139 -98.32111) usCentral
  , loc "Rhode Island" "Cranston" (latlon 41.783333 -71.441666) usEastern
  , loc "California" "Lakewood" (latlon 33.8475 -118.12) usPacific
  , loc "Michigan" "Farmington Hills" (latlon 42.48528 -83.376945) usEastern
  , loc "Missouri" "O'Fallon" (latlon 38.784443 -90.70805) usCentral
  , loc "California" "Alameda" (latlon 37.76389 -122.25694) usPacific
  , loc "North Carolina" "Concord" (latlon 35.4044 -80.6006) usEastern
  , loc "California" "Merced" (latlon 37.302223 -120.483055) usPacific
  , loc "Illinois" "Bloomington" (latlon 40.484165 -88.993614) usCentral
  , loc "Wisconsin" "Racine" (latlon 42.726112 -87.80583) usCentral
  , loc "California" "Hemet" (latlon 33.741943 -116.983055) usPacific
  , loc "Massachusetts" "Lawrence" (latlon 42.706944 -71.16361) usEastern
  , loc "California" "Chino" (latlon 34.017776 -117.69) usPacific
  , loc "Florida" "Largo" (latlon 27.909166 -82.7875) usEastern
  , loc "California" "Menifee" (latlon 33.678333 -117.16695) usPacific
  , loc "Georgia" "Albany" (latlon 31.582222 -84.16556) usEastern
  , loc "New Jersey" "Camden" (latlon 39.9368 -75.1066) usEastern
  , loc "California" "Lake Forest" (latlon 33.641666 -117.690834) usPacific
  , loc "New York" "New Rochelle" (latlon 40.92861 -73.784164) usEastern
  , loc "California" "Napa" (latlon 38.3 -122.3) usPacific
  , loc "Utah" "St. George" (latlon 37.095276 -113.57806) usMountain
  , loc "California" "Redwood" (latlon 37.482777 -122.236115) usPacific
  , loc "Missouri" "St. Joseph" (latlon 39.758057 -94.83667) usCentral
  , loc "Georgia" "Johns Creek" (latlon 34.0289 -84.1986) usEastern
  , loc "Oregon" "Bend" (latlon 44.05639 -121.30805) usPacific
  , loc "California" "Bellflower" (latlon 33.888054 -118.1275) usPacific
  , loc "Arizona" "Avondale" (latlon 33.433613 -112.349724) usMountain
  , loc "Texas" "Bryan" (latlon 30.6656 -96.3667) usCentral
  , loc "Illinois" "Decatur" (latlon 39.851665 -88.94417) usCentral
  , loc "Florida" "Melbourne" (latlon 28.116667 -80.63333) usEastern
  , loc "California" "Indio" (latlon 33.72 -116.23194) usPacific
  , loc "Minnesota" "Brooklyn Park" (latlon 45.094166 -93.35611) usCentral
  , loc "California" "Tustin" (latlon 33.739723 -117.813614) usPacific
  , loc "Illinois" "Evanston" (latlon 42.04114 -87.690056) usCentral
  , loc "California" "Baldwin Park" (latlon 34.08278 -117.97111) usPacific
  , loc "Florida" "Palm Coast" (latlon 29.538055 -81.223335) usEastern
  , loc "Idaho" "Meridian" (latlon 43.614166 -116.39889) usMountain
  , loc "Florida" "Deerfield Beach" (latlon 26.318056 -80.099724) usEastern
  , loc "Oregon" "Medford" (latlon 42.33194 -122.86167) usPacific
  , loc "California" "Chino Hills" (latlon 33.97 -117.75) usPacific
  , loc "Michigan" "Kalamazoo" (latlon 42.29 -85.59) usEastern
  , loc "California" "Mountain View" (latlon 37.392776 -122.04195) usPacific
  , loc "California" "San Ramon" (latlon 37.78 -121.97806) usPacific
  , loc "Washington" "Kennewick" (latlon 46.203476 -119.15927) usPacific
  , loc "California" "Upland" (latlon 34.0999 -117.647) usPacific
  , loc "Arkansas" "Fayetteville" (latlon 36.07639 -94.160835) usCentral
  , loc "Connecticut" "New Britain" (latlon 41.675 -72.787224) usEastern
  , loc "Ohio" "Canton" (latlon 40.805 -81.37583) usEastern
  , loc "California" "Union" (latlon 37.59639 -122.04833) usPacific
  , loc "Wisconsin" "Appleton" (latlon 44.282223 -88.418335) usCentral
  , loc "California" "Folsom" (latlon 38.672222 -121.157776) usPacific
  , loc "Michigan" "Wyoming" (latlon 42.913612 -85.70556) usEastern
  , loc "Louisiana" "Lake Charles" (latlon 30.214722 -93.20861) usCentral
  , loc "Texas" "Baytown" (latlon 29.743889 -94.965836) usCentral
  , loc "North Carolina" "Gastonia" (latlon 35.25528 -81.180275) usEastern
  , loc "Michigan" "Southfield" (latlon 42.47333 -83.22195) usEastern
  , loc "Texas" "Conroe" (latlon 30.3161 -95.4589) usCentral
  , loc "Texas" "Pearsall" (latlon 28.8914 -99.095) usCentral
  , loc "Delaware" "Wilmington" (latlon 39.748333 -75.55139) usEastern
  , loc "Rhode Island" "Pawtucket" (latlon 41.875557 -71.376114) usEastern
  , loc "Michigan" "Rochester Hills" (latlon 42.658054 -83.14972) usEastern
  , loc "South Dakota" "Rapid" (latlon 44.076187 -103.228294) usMountain
  , loc "Wisconsin" "Waukesha" (latlon 43.011665 -88.23167) usCentral
  , loc "Minnesota" "Plymouth" (latlon 45.010555 -93.45556) usCentral
  , loc "Texas" "Pharr" (latlon 26.206388 -98.18528) usCentral
  , loc "California" "Pleasanton" (latlon 37.6725 -121.8825) usPacific
  , loc "Arkansas" "Jonesboro" (latlon 35.833332 -90.7) usCentral
  , loc "North Carolina" "Jacksonville" (latlon 34.759724 -77.40972) usEastern
  , loc "California" "Milpitas" (latlon 37.434723 -121.895) usPacific
  , loc "Indiana" "Muncie" (latlon 40.193333 -85.388054) usEastern
  , loc "Montana" "Missoula" (latlon 46.8625 -114.011665) usMountain
  , loc "Arkansas" "Springdale" (latlon 36.18139 -94.145836) usCentral
  , loc "New Jersey" "Passaic" (latlon 40.8575 -74.12889) usEastern
  , loc "California" "Lynwood" (latlon 33.92472 -118.20194) usPacific
  , loc "Tennessee" "Franklin" (latlon 35.929165 -86.8575) usCentral
  , loc "California" "Redlands" (latlon 34.05472 -117.1825) usPacific
  , loc "Arizona" "Flagstaff" (latlon 35.199165 -111.63111) usMountain
  , loc "California" "Turlock" (latlon 37.505833 -120.84889) usPacific
  , loc "Iowa" "Waterloo" (latlon 42.492435 -92.34616) usCentral
  , loc "California" "Perris" (latlon 33.7825 -117.228615) usPacific
  , loc "Florida" "Boynton Beach" (latlon 26.528055 -80.076385) usEastern
  , loc "Iowa" "Iowa" (latlon 41.655834 -91.525) usCentral
  ]
  where
    loc = Location "USA"

usStateAbbreviations :: M.Map String String
usStateAbbreviations =
  M.fromList
    [ ("Alabama", "AL")
    , ("Alaska", "AK")
    , ("Arizona", "AZ")
    , ("Arkansas", "AR")
    , ("California", "CA")
    , ("Colorado", "CO")
    , ("Connecticut", "CT")
    , ("Delaware", "DE")
    , ("Florida", "FL")
    , ("Georgia", "GA")
    , ("Hawaii", "HI")
    , ("Idaho", "ID")
    , ("Illinois", "IL")
    , ("Indiana", "IN")
    , ("Iowa", "IA")
    , ("Kansas", "KS")
    , ("Kentucky", "KY")
    , ("Louisiana", "LA")
    , ("Maine", "ME")
    , ("Maryland", "MD")
    , ("Massachusetts", "MA")
    , ("Michigan", "MI")
    , ("Minnesota", "MN")
    , ("Mississippi", "MS")
    , ("Missouri", "MO")
    , ("Montana", "MT")
    , ("Nebraska", "NE")
    , ("Nevada", "NV")
    , ("New Hampshire", "NH")
    , ("New Jersey", "NJ")
    , ("New Mexico", "NM")
    , ("New York", "NY")
    , ("North Carolina", "NC")
    , ("North Dakota", "ND")
    , ("Ohio", "OH")
    , ("Oklahoma", "OK")
    , ("Oregon", "OR")
    , ("Pennsylvania", "PA")
    , ("Rhode Island", "RI")
    , ("South Carolina", "SC")
    , ("South Dakota", "SD")
    , ("Tennessee", "TN")
    , ("Texas", "TX")
    , ("Utah", "UT")
    , ("Vermont", "VT")
    , ("Virginia", "VA")
    , ("Washington", "WA")
    , ("West Virginia", "WV")
    , ("Wisconsin", "WI")
    , ("Wyoming", "WY")
    , ("American Samoa", "AS")
    , ("District of Columbia", "DC")
    , ("Federated States of Micronesia", "FM")
    , ("Guam", "GU")
    , ("Marshall Islands", "MH")
    , ("Northern Mariana Islands", "MP")
    , ("Palau", "PW")
    , ("Puerto Rico", "PR")
    , ("Virgin Islands", "VI")
    ]

usStateAbbreviation :: String -> String
usStateAbbreviation state =
  fromMaybe (error $ "Invalid state " ++ state) $
  M.lookup state usStateAbbreviations
