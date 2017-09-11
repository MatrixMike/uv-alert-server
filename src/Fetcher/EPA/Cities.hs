{-# LANGUAGE NegativeLiterals #-}

module Fetcher.EPA.Cities
  ( cities
  , usStateAbbreviation
  ) where

import qualified Data.Map as M
import Data.Maybe

import Types.Location
import Types.Location.USA

cities :: [LocationCoordinates]
cities = [
         loc "New York" "New York" (latlon 40.67 -73.94) eastern
        , loc "California" "Los Angeles" (latlon 34.05 -118.25) pacific
        , loc "Illinois" "Chicago" (latlon 41.9 -87.65) central
        , loc "Texas" "Houston" (latlon 29.762777 -95.38306) central
        , loc "Arizona" "Phoenix" (latlon 33.52833 -112.076385) mountain
        , loc "Texas" "San Antonio" (latlon 29.428612 -98.49333) central
        , loc "Texas" "Dallas" (latlon 32.7825 -96.7975) central
        , loc "California" "San Jose" (latlon 37.304165 -121.87278) pacific
        , loc "Texas" "Austin" (latlon 30.3 -97.73333) central
        , loc "Indiana" "Indianapolis" (latlon 39.767776 -86.15806) eastern
        , loc "Florida" "Jacksonville" (latlon 30.316668 -81.65) eastern
        , loc "Texas" "Fort Worth" (latlon 32.75722 -97.33305) central
        , loc "California" "San Francisco" (latlon 37.766666 -122.433334) pacific
        , loc "Ohio" "Columbus" (latlon 39.983334 -82.98333) eastern
        , loc "Kentucky" "Louisville" (latlon 38.254166 -85.76028) eastern
        , loc "Michigan" "Detroit" (latlon 42.331665 -83.0475) eastern
        , loc "Tennessee" "Nashville" (latlon 36.165 -86.78389) central
        , loc "Massachusetts" "Boston" (latlon 42.357777 -71.06167) eastern
        , loc "Colorado" "Denver" (latlon 39.739166 -104.984726) mountain
        , loc "Texas" "El Paso" (latlon 31.790277 -106.42333) mountain
        , loc "Tennessee" "Memphis" (latlon 35.1175 -89.97111) central
        , loc "Maryland" "Baltimore" (latlon 39.28639 -76.615) eastern
        , loc "Washington" "Seattle" (latlon 47.6 -122.316666) pacific
        , loc "Oklahoma" "Oklahoma" (latlon 35.4823 -97.5352) central
        , loc "Oregon" "Portland" (latlon 45.516666 -122.666664) pacific
        , loc "New Mexico" "Albuquerque" (latlon 35.116665 -106.61667) mountain
        , loc "California" "Fresno" (latlon 36.781666 -119.79222) pacific
        , loc "Missouri" "Kansas" (latlon 39.05 -94.583336) central
        , loc "Georgia" "Atlanta" (latlon 33.756943 -84.390274) eastern
        , loc "North Carolina" "Raleigh" (latlon 35.81889 -78.64472) eastern
        , loc "Arizona" "Mesa" (latlon 33.415 -111.83139) mountain
        , loc "Colorado" "Colorado Springs" (latlon 38.863335 -104.79195) mountain
        , loc "Ohio" "Cleveland" (latlon 41.482224 -81.66972) eastern
        , loc "Oklahoma" "Tulsa" (latlon 36.13139 -95.937225) central
        , loc "Kansas" "Wichita" (latlon 37.68889 -97.33611) central
        , loc "Louisiana" "New Orleans" (latlon 29.966667 -90.05) central
        , loc "Texas" "Arlington" (latlon 32.705032 -97.12284) central
        , loc "Florida" "Tampa" (latlon 27.970833 -82.46472) eastern
        , loc "California" "Bakersfield" (latlon 35.405834 -119.01861) pacific
        , loc "California" "Anaheim" (latlon 33.836113 -117.889725) pacific
        , loc "Colorado" "Aurora" (latlon 39.695835 -104.80805) mountain
        , loc "California" "Santa Ana" (latlon 33.740833 -117.881386) pacific
        , loc "Missouri" "St. Louis" (latlon 38.616665 -90.2) central
        , loc "Texas" "Corpus Christi" (latlon 27.742777 -97.40195) central
        , loc "California" "Riverside" (latlon 33.948063 -117.396126) pacific
        , loc "Alaska" "Anchorage" (latlon 61.218334 -149.89917) alaska
        , loc "Kentucky" "Lexington" (latlon 38.029724 -84.49472) eastern
        , loc "California" "Stockton" (latlon 37.975555 -121.300835) pacific
        , loc "Minnesota" "Saint Paul" (latlon 44.944168 -93.09361) central
        , loc "New Jersey" "Newark" (latlon 40.73528 -74.185) eastern
        , loc "Nebraska" "Lincoln" (latlon 40.810555 -96.680275) central
        , loc "North Carolina" "Greensboro" (latlon 36.08 -79.81944) eastern
        , loc "Texas" "Plano" (latlon 33.05 -96.75) central
        , loc "Nevada" "Henderson" (latlon 36.0292 -115.0253) pacific
        , loc "Indiana" "Fort Wayne" (latlon 41.08045 -85.13915) eastern
        , loc "California" "Irvine" (latlon 33.684166 -117.7925) pacific
        , loc "New Jersey" "Jersey" (latlon 40.711388 -74.06472) eastern
        , loc "Texas" "Laredo" (latlon 27.506111 -99.507225) central
        , loc "California" "Chula Vista" (latlon 32.627777 -117.04806) pacific
        , loc "Texas" "Lubbock" (latlon 33.566666 -101.88333) central
        , loc "Florida" "Orlando" (latlon 28.533611 -81.386665) eastern
        , loc "Wisconsin" "Madison" (latlon 43.074722 -89.384445) central
        , loc "North Carolina" "Winston-Salem" (latlon 36.1025 -80.26056) eastern
        , loc "Arizona" "Chandler" (latlon 33.303333 -111.84139) mountain
        , loc "Louisiana" "Baton Rouge" (latlon 30.416666 -91.1) central
        , loc "North Carolina" "Durham" (latlon 35.9875 -78.90722) eastern
        , loc "Texas" "Garland" (latlon 32.906944 -96.635) central
        , loc "Arizona" "Glendale" (latlon 33.583332 -112.2) mountain
        , loc "Nevada" "Reno" (latlon 39.52722 -119.821945) pacific
        , loc "Florida" "Hialeah" (latlon 25.860556 -80.29389) eastern
        , loc "California" "Fremont" (latlon 37.543056 -121.98278) pacific
        , loc "California" "Santa Clarita" (latlon 34.41639 -118.506386) pacific
        , loc "Arizona" "Scottsdale" (latlon 33.493057 -111.92611) mountain
        , loc "Nevada" "North Las Vegas" (latlon 36.22861 -115.14667) pacific
        , loc "Texas" "Irving" (latlon 32.81167 -96.950836) central
        , loc "New York" "Rochester" (latlon 43.165554 -77.61139) eastern
        , loc "Alabama" "Montgomery" (latlon 32.361668 -86.27917) central
        , loc "Idaho" "Boise" (latlon 43.61361 -116.23778) mountain
        , loc "California" "Oxnard" (latlon 34.191387 -119.1825) pacific
        , loc "North Carolina" "Fayetteville" (latlon 35.066666 -78.9175) eastern
        , loc "Iowa" "Des Moines" (latlon 41.59083 -93.620834) central
        , loc "California" "Fontana" (latlon 34.1 -117.46667) pacific
        , loc "California" "Modesto" (latlon 37.66139 -120.994446) pacific
        , loc "Georgia" "Columbus" (latlon 32.49222 -84.94028) eastern
        , loc "Louisiana" "Shreveport" (latlon 32.50821 -93.762955) central
        , loc "Washington" "Tacoma" (latlon 47.24139 -122.45944) pacific
        , loc "Illinois" "Aurora" (latlon 41.76 -88.298615) central
        , loc "Georgia" "Augusta" (latlon 33.47 -81.975) eastern
        , loc "Alabama" "Mobile" (latlon 30.727669 -88.05267) central
        , loc "California" "Moreno Valley" (latlon 33.943054 -117.22833) pacific
        , loc "California" "Glendale" (latlon 34.170834 -118.25) pacific
        , loc "Texas" "Amarillo" (latlon 35.199165 -101.845276) central
        , loc "California" "Huntington Beach" (latlon 33.69278 -117.999725) pacific
        , loc "Utah" "Salt Lake" (latlon 40.75 -111.88333) mountain
        , loc "Texas" "Brownsville" (latlon 25.930277 -97.48444) central
        , loc "Massachusetts" "Worcester" (latlon 42.266666 -71.8) eastern
        , loc "Florida" "Tallahassee" (latlon 30.438736 -84.28063) eastern
        , loc "Alabama" "Huntsville" (latlon 34.71361 -86.58611) central
        , loc "Tennessee" "Knoxville" (latlon 35.966667 -83.95) eastern
        , loc "Texas" "Grand Prairie" (latlon 32.71528 -97.016945) central
        , loc "California" "Oceanside" (latlon 33.211666 -117.325836) pacific
        , loc "Mississippi" "Jackson" (latlon 32.29889 -90.18472) central
        , loc "Kansas" "Overland Park" (latlon 38.94007 -94.680695) central
        , loc "Tennessee" "Chattanooga" (latlon 35.045555 -85.26722) eastern
        , loc "Florida" "Fort Lauderdale" (latlon 26.135834 -80.141945) eastern
        , loc "California" "Garden Grove" (latlon 33.77889 -117.96028) pacific
        , loc "California" "Santa Rosa" (latlon 38.448612 -122.70472) pacific
        , loc "California" "Rancho Cucamonga" (latlon 34.123333 -117.579445) pacific
        , loc "South Dakota" "Sioux Falls" (latlon 43.53639 -96.73167) central
        , loc "Florida" "Port St. Lucie" (latlon 27.275833 -80.355) eastern
        , loc "Washington" "Vancouver" (latlon 45.63361 -122.602776) pacific
        , loc "Arizona" "Tempe" (latlon 33.429443 -111.943054) mountain
        , loc "California" "Corona" (latlon 33.866665 -117.566666) pacific
        , loc "Missouri" "Springfield" (latlon 37.195 -93.28611) central
        , loc "California" "Lancaster" (latlon 34.686943 -118.15417) pacific
        , loc "Texas" "McKinney" (latlon 33.2 -96.63333) central
        , loc "Florida" "Pembroke Pines" (latlon 26.0125 -80.313614) eastern
        , loc "Oregon" "Salem" (latlon 44.930832 -123.02889) pacific
        , loc "Florida" "Cape Coral" (latlon 26.633333 -81.98333) eastern
        , loc "Arizona" "Peoria" (latlon 33.5825 -112.23861) mountain
        , loc "Georgia" "Macon" (latlon 32.83583 -83.64639) eastern
        , loc "Massachusetts" "Springfield" (latlon 42.11241 -72.547455) eastern
        , loc "California" "Elk Grove" (latlon 38.43833 -121.38194) pacific
        , loc "California" "Palmdale" (latlon 34.581112 -118.100555) pacific
        , loc "California" "Salinas" (latlon 36.677776 -121.655556) pacific
        , loc "California" "Hayward" (latlon 37.668888 -122.08083) pacific
        , loc "California" "Pomona" (latlon 34.060833 -117.75584) pacific
        , loc "Texas" "Pasadena" (latlon 29.676111 -95.17389) central
        , loc "Virginia" "Alexandria" (latlon 38.80472 -77.047226) eastern
        , loc "New Jersey" "Paterson, New Jersey" (latlon 40.915554 -74.163055) eastern
        , loc "Kansas" "Kansas" (latlon 39.106667 -94.67639) central
        , loc "California" "Torrance" (latlon 33.83585 -118.34063) pacific
        , loc "Colorado" "Fort Collins" (latlon 40.566666 -105.083336) mountain
        , loc "California" "Escondido" (latlon 33.12472 -117.08083) pacific
        , loc "Texas" "Mesquite" (latlon 32.78278 -96.60972) central
        , loc "Colorado" "Lakewood" (latlon 39.70639 -105.102776) mountain
        , loc "Georgia" "Savannah" (latlon 32.050835 -81.10389) eastern
        , loc "Illinois" "Naperville" (latlon 41.748055 -88.16556) central
        , loc "Ohio" "Dayton" (latlon 39.766666 -84.2) eastern
        , loc "Texas" "McAllen" (latlon 26.216389 -98.23639) central
        , loc "California" "Sunnyvale" (latlon 37.36889 -122.03694) pacific
        , loc "Texas" "Huntsville" (latlon 30.723328 -95.55096) central
        , loc "Texas" "Killeen" (latlon 31.105556 -97.72667) central
        , loc "California" "Orange" (latlon 33.803055 -117.8325) pacific
        , loc "California" "Fullerton" (latlon 33.88 -117.92861) pacific
        , loc "Michigan" "Warren" (latlon 42.491943 -83.02389) eastern
        , loc "Tennessee" "Clarksville" (latlon 36.52972 -87.35944) central
        , loc "Utah" "West Valley" (latlon 40.689167 -111.99389) mountain
        , loc "California" "Visalia" (latlon 36.317223 -119.33195) pacific
        , loc "Connecticut" "New Haven" (latlon 41.308334 -72.925) eastern
        , loc "Michigan" "Sterling Heights" (latlon 42.580276 -83.03028) eastern
        , loc "Florida" "Miramar" (latlon 25.97889 -80.2825) eastern
        , loc "Florida" "Gainesville" (latlon 29.665277 -82.33611) eastern
        , loc "Connecticut" "Stamford" (latlon 41.09667 -73.55222) eastern
        , loc "Kansas" "Topeka" (latlon 39.033333 -95.683334) central
        , loc "Texas" "Carrollton" (latlon 32.99 -96.89333) central
        , loc "California" "Thousand Oaks" (latlon 34.189445 -118.875) pacific
        , loc "Iowa" "Cedar Rapids" (latlon 41.983334 -91.66861) central
        , loc "New Jersey" "Elizabeth" (latlon 40.662224 -74.20917) eastern
        , loc "California" "Concord" (latlon 37.978054 -122.03111) pacific
        , loc "Kansas" "Olathe" (latlon 38.8808 -94.8031) central
        , loc "Connecticut" "Hartford" (latlon 41.763332 -72.685) eastern
        , loc "Texas" "Waco" (latlon 31.55139 -97.15583) central
        , loc "California" "Simi Valley" (latlon 34.270832 -118.73917) pacific
        , loc "Texas" "Midland" (latlon 32.005 -102.09917) central
        , loc "Washington" "Bellevue" (latlon 47.5975 -122.159164) pacific
        , loc "Florida" "Coral Springs" (latlon 26.270555 -80.25916) eastern
        , loc "Louisiana" "Lafayette" (latlon 30.216667 -92.03333) central
        , loc "South Carolina" "Charleston" (latlon 32.77611 -79.9325) eastern
        , loc "Florida" "Panama Beach" (latlon 30.176666 -85.80556) central
        , loc "Colorado" "Thornton" (latlon 39.9031 -104.954) mountain
        , loc "Texas" "Beaumont" (latlon 30.08 -94.12666) central
        , loc "Arizona" "Surprise" (latlon 33.629196 -112.3678) mountain
        , loc "Indiana" "Evansville" (latlon 37.974724 -87.55583) central
        , loc "Texas" "Abilene" (latlon 32.44639 -99.74555) central
        , loc "Texas" "Frisco" (latlon 33.1414 -96.8131) central
        , loc "Utah" "Provo" (latlon 40.244446 -111.660835) mountain
        , loc "California" "Vallejo" (latlon 38.104088 -122.25664) pacific
        , loc "California" "Victorville" (latlon 34.53611 -117.28833) pacific
        , loc "Illinois" "Peoria" (latlon 40.720833 -89.60944) central
        , loc "Georgia" "Athens" (latlon 33.955276 -83.38306) eastern
        , loc "Michigan" "Lansing" (latlon 42.7335 -84.5467) eastern
        , loc "California" "El Monte" (latlon 34.073334 -118.0275) pacific
        , loc "Texas" "Denton" (latlon 33.216667 -97.13333) central
        , loc "California" "Berkeley" (latlon 37.870277 -122.26806) pacific
        , loc "Michigan" "Grand Rapids" (latlon 42.96125 -85.655716) eastern
        , loc "California" "Downey" (latlon 33.938057 -118.13084) pacific
        , loc "Oklahoma" "Norman" (latlon 35.22167 -97.418335) central
        , loc "Minnesota" "Rochester" (latlon 44.0234 -92.46295) central
        , loc "Texas" "Odessa" (latlon 31.863333 -102.365555) central
        , loc "Connecticut" "Waterbury" (latlon 41.558334 -73.03694) eastern
        , loc "Illinois" "Elgin" (latlon 42.039444 -88.28861) central
        , loc "California" "Costa Mesa" (latlon 33.67 -117.91) pacific
        , loc "Massachusetts" "Lowell" (latlon 42.639446 -71.31472) eastern
        , loc "Texas" "Round Rock" (latlon 30.515 -97.6725) central
        , loc "California" "Inglewood" (latlon 33.9575 -118.34611) pacific
        , loc "New Hampshire" "Manchester" (latlon 42.990833 -71.46361) eastern
        , loc "Tennessee" "Murfreesboro" (latlon 35.84614 -86.392075) central
        , loc "Texas" "Pearland" (latlon 29.5544 -95.2958) central
        , loc "Missouri" "Columbia" (latlon 38.948334 -92.333885) central
        , loc "Utah" "West Jordan" (latlon 40.606388 -111.97611) mountain
        , loc "Florida" "Clearwater" (latlon 27.973612 -82.76417) eastern
        , loc "Texas" "Tyler" (latlon 32.35 -95.3) central
        , loc "Massachusetts" "Cambridge" (latlon 42.375 -71.10611) eastern
        , loc "Florida" "Miami Gardens" (latlon 25.942122 -80.26992) eastern
        , loc "Colorado" "Pueblo" (latlon 38.266945 -104.62028) mountain
        , loc "Colorado" "Arvada" (latlon 39.802765 -105.08749) mountain
        , loc "California" "Ventura" (latlon 34.275 -119.227776) pacific
        , loc "Colorado" "Westminster" (latlon 39.836113 -105.037224) mountain
        , loc "California" "West Covina" (latlon 34.056667 -117.91861) pacific
        , loc "California" "Norwalk" (latlon 33.906944 -118.083336) pacific
        , loc "North Dakota" "Fargo" (latlon 46.877224 -96.789444) central
        , loc "California" "Fairfield" (latlon 38.25778 -122.05417) pacific
        , loc "California" "Roseville" (latlon 38.7525 -121.289444) pacific
        , loc "Texas" "Wichita Falls" (latlon 33.896946 -98.515) central
        , loc "North Carolina" "High Point" (latlon 35.970554 -79.9975) eastern
        , loc "Wisconsin" "Green Bay" (latlon 44.513332 -88.01583) central
        , loc "Florida" "West Palm Beach" (latlon 26.709723 -80.06416) eastern
        , loc "California" "Richmond" (latlon 37.935833 -122.34778) pacific
        , loc "California" "Murrieta" (latlon 33.569443 -117.2025) pacific
        , loc "California" "Burbank" (latlon 34.18028 -118.32833) pacific
        , loc "Florida" "Palm Bay" (latlon 27.997923 -80.670006) eastern
        , loc "Washington" "Everett" (latlon 47.963333 -122.200554) pacific
        , loc "Michigan" "Flint" (latlon 43.01 -83.69) eastern
        , loc "California" "Antioch" (latlon 38.005 -121.80583) pacific
        , loc "Georgia" "Sandy Springs" (latlon 33.9375 -84.368614) eastern
        , loc "Indiana" "South Bend" (latlon 41.6725 -86.25528) eastern
        , loc "California" "Daly" (latlon 37.68639 -122.46833) pacific
        , loc "Florida" "Lakeland" (latlon 28.041111 -81.958885) eastern
        , loc "Colorado" "Centennial" (latlon 39.59639 -104.84389) mountain
        , loc "New York" "Albany" (latlon 42.65 -73.76667) eastern
        , loc "California" "Temecula" (latlon 33.503334 -117.12361) pacific
        , loc "Texas" "College Station" (latlon 30.601389 -96.314445) central
        , loc "Florida" "Pompano Beach" (latlon 26.234722 -80.12556) eastern
        , loc "California" "Santa Maria" (latlon 34.95139 -120.433334) pacific
        , loc "California" "El Cajon" (latlon 32.798332 -116.96) pacific
        , loc "Texas" "Richardson" (latlon 32.965557 -96.715836) central
        , loc "Wisconsin" "Kenosha" (latlon 42.582222 -87.84556) central
        , loc "Texas" "Allen" (latlon 33.099724 -96.663055) central
        , loc "California" "Rialto" (latlon 34.11139 -117.3825) pacific
        , loc "Oklahoma" "Broken Arrow" (latlon 36.03639 -95.78361) central
        , loc "California" "Jurupa Valley" (latlon 33.9994 -117.475) pacific
        , loc "Michigan" "Dearborn" (latlon 42.31133 -83.21348) eastern
        , loc "New Mexico" "Las Cruces" (latlon 32.31972 -106.765274) mountain
        , loc "South Carolina" "North Charleston" (latlon 32.885277 -80.016945) eastern
        , loc "Colorado" "Boulder" (latlon 40.019444 -105.29278) mountain
        , loc "California" "San Mateo" (latlon 37.554165 -122.31306) pacific
        , loc "Michigan" "Livonia" (latlon 42.39722 -83.37361) eastern
        , loc "Michigan" "Livonia" (latlon 42.40111 -83.37111) eastern
        , loc "California" "Compton" (latlon 33.896667 -118.225) pacific
        , loc "California" "Clovis" (latlon 36.82528 -119.70306) pacific
        , loc "Texas" "Lewisville" (latlon 33.0383 -97.0061) central
        , loc "Massachusetts" "New Bedford" (latlon 41.636112 -70.93472) eastern
        , loc "Georgia" "Roswell" (latlon 34.0339 -84.3442) eastern
        , loc "California" "Vista" (latlon 33.19361 -117.24111) pacific
        , loc "Massachusetts" "Brockton" (latlon 42.083332 -71.01889) eastern
        , loc "California" "Mission Viejo" (latlon 33.612778 -117.65611) pacific
        , loc "Texas" "San Angelo" (latlon 31.45 -100.45) central
        , loc "Arizona" "Yuma" (latlon 32.692223 -114.61528) mountain
        , loc "Colorado" "Greeley" (latlon 40.416668 -104.7) mountain
        , loc "California" "Vacaville" (latlon 38.35389 -121.97278) pacific
        , loc "Massachusetts" "Quincy" (latlon 42.25 -71.0) eastern
        , loc "California" "Carson" (latlon 33.83972 -118.25972) pacific
        , loc "Oregon" "Hillsboro" (latlon 45.523056 -122.988335) pacific
        , loc "Missouri" "Lee's Summit" (latlon 38.9225 -94.37417) central
        , loc "Washington" "Yakima" (latlon 46.601944 -120.507774) pacific
        , loc "Washington" "Renton" (latlon 47.486668 -122.195274) pacific
        , loc "Utah" "Orem" (latlon 40.29875 -111.69649) mountain
        , loc "Alabama" "Tuscaloosa" (latlon 33.206665 -87.53472) central
        , loc "Massachusetts" "Lynn" (latlon 42.466667 -70.95) eastern
        , loc "California" "Hesperia" (latlon 34.412777 -117.306114) pacific
        , loc "Oregon" "Beaverton" (latlon 45.486946 -122.80361) pacific
        , loc "Washington" "Spokane Valley" (latlon 47.6733 -117.239) pacific
        , loc "California" "Santa Monica" (latlon 34.021946 -118.48139) pacific
        , loc "California" "Westminster" (latlon 33.75139 -117.99389) pacific
        , loc "Utah" "Sandy" (latlon 40.5725 -111.859726) mountain
        , loc "Massachusetts" "Fall River" (latlon 41.70139 -71.155556) eastern
        , loc "Florida" "Sunrise" (latlon 26.157223 -80.28611) eastern
        , loc "Connecticut" "Norwalk" (latlon 41.093887 -73.41972) eastern
        , loc "Kansas" "Lawrence" (latlon 38.97167 -95.235275) central
        , loc "New Mexico" "Rio Rancho" (latlon 35.28611 -106.670555) mountain
        , loc "California" "Livermore" (latlon 37.685555 -121.76417) pacific
        , loc "New Hampshire" "Nashua" (latlon 42.7575 -71.46445) eastern
        , loc "Colorado" "Longmont" (latlon 40.171665 -105.10917) mountain
        , loc "California" "Chico" (latlon 39.74 -121.835556) pacific
        , loc "Indiana" "Carmel" (latlon 39.966667 -86.1) eastern
        , loc "California" "Whittier" (latlon 33.965557 -118.024445) pacific
        , loc "California" "Newport Beach" (latlon 33.616665 -117.8975) pacific
        , loc "Florida" "Deltona" (latlon 28.905 -81.21111) eastern
        , loc "California" "San Leandro" (latlon 37.725 -122.15611) pacific
        , loc "New Jersey" "Trenton" (latlon 40.22167 -74.75611) eastern
        , loc "Illinois" "Champaign" (latlon 40.112778 -88.26111) central
        , loc "Alabama" "Hoover" (latlon 33.386112 -86.80556) central
        , loc "Michigan" "Westland" (latlon 42.324165 -83.400276) eastern
        , loc "Texas" "Sugar Land" (latlon 29.599445 -95.61417) central
        , loc "Utah" "Ogden" (latlon 41.22778 -111.96111) mountain
        , loc "California" "San Marcos" (latlon 33.141945 -117.17028) pacific
        , loc "Texas" "League" (latlon 29.499722 -95.08972) central
        , loc "Idaho" "Nampa" (latlon 43.574722 -116.563614) mountain
        , loc "California" "Citrus Heights" (latlon 38.69 -121.29) pacific
        , loc "California" "Alhambra" (latlon 34.081944 -118.135) pacific
        , loc "California" "Tracy" (latlon 37.738056 -121.43389) pacific
        , loc "Minnesota" "Bloomington" (latlon 44.83361 -93.31) central
        , loc "Rhode Island" "Warwick" (latlon 41.716667 -71.416664) eastern
        , loc "Connecticut" "Danbury" (latlon 41.40222 -73.47111) eastern
        , loc "Ohio" "Parma" (latlon 41.391666 -81.727776) eastern
        , loc "Oklahoma" "Edmond" (latlon 35.65 -97.45) central
        , loc "Texas" "Edinburg" (latlon 26.3042 -98.1639) central
        , loc "Michigan" "Troy" (latlon 42.580276 -83.14306) eastern
        , loc "Washington" "Bellingham" (latlon 48.75028 -122.475) pacific
        , loc "Indiana" "Hammond" (latlon 41.6111 -87.4931) central
        , loc "California" "Buena Park" (latlon 33.85611 -118.004166) pacific
        , loc "Texas" "Longview" (latlon 32.5092 -94.7539) central
        , loc "Texas" "Mission" (latlon 26.21139 -98.32111) central
        , loc "Rhode Island" "Cranston" (latlon 41.783333 -71.441666) eastern
        , loc "California" "Lakewood" (latlon 33.8475 -118.12) pacific
        , loc "Michigan" "Farmington Hills" (latlon 42.48528 -83.376945) eastern
        , loc "Missouri" "O'Fallon" (latlon 38.784443 -90.70805) central
        , loc "California" "Alameda" (latlon 37.76389 -122.25694) pacific
        , loc "North Carolina" "Concord" (latlon 35.4044 -80.6006) eastern
        , loc "California" "Merced" (latlon 37.302223 -120.483055) pacific
        , loc "Illinois" "Bloomington" (latlon 40.484165 -88.993614) central
        , loc "Wisconsin" "Racine" (latlon 42.726112 -87.80583) central
        , loc "California" "Hemet" (latlon 33.741943 -116.983055) pacific
        , loc "Massachusetts" "Lawrence" (latlon 42.706944 -71.16361) eastern
        , loc "California" "Chino" (latlon 34.017776 -117.69) pacific
        , loc "Florida" "Largo" (latlon 27.909166 -82.7875) eastern
        , loc "California" "Menifee" (latlon 33.678333 -117.16695) pacific
        , loc "Georgia" "Albany" (latlon 31.582222 -84.16556) eastern
        , loc "New Jersey" "Camden" (latlon 39.9368 -75.1066) eastern
        , loc "California" "Lake Forest" (latlon 33.641666 -117.690834) pacific
        , loc "New York" "New Rochelle" (latlon 40.92861 -73.784164) eastern
        , loc "California" "Napa" (latlon 38.3 -122.3) pacific
        , loc "Utah" "St. George" (latlon 37.095276 -113.57806) mountain
        , loc "California" "Redwood" (latlon 37.482777 -122.236115) pacific
        , loc "Missouri" "St. Joseph" (latlon 39.758057 -94.83667) central
        , loc "Georgia" "Johns Creek" (latlon 34.0289 -84.1986) eastern
        , loc "Oregon" "Bend" (latlon 44.05639 -121.30805) pacific
        , loc "California" "Bellflower" (latlon 33.888054 -118.1275) pacific
        , loc "Arizona" "Avondale" (latlon 33.433613 -112.349724) mountain
        , loc "Texas" "Bryan" (latlon 30.6656 -96.3667) central
        , loc "Illinois" "Decatur" (latlon 39.851665 -88.94417) central
        , loc "Florida" "Melbourne" (latlon 28.116667 -80.63333) eastern
        , loc "California" "Indio" (latlon 33.72 -116.23194) pacific
        , loc "Minnesota" "Brooklyn Park" (latlon 45.094166 -93.35611) central
        , loc "California" "Tustin" (latlon 33.739723 -117.813614) pacific
        , loc "Illinois" "Evanston" (latlon 42.04114 -87.690056) central
        , loc "California" "Baldwin Park" (latlon 34.08278 -117.97111) pacific
        , loc "Florida" "Palm Coast" (latlon 29.538055 -81.223335) eastern
        , loc "Idaho" "Meridian" (latlon 43.614166 -116.39889) mountain
        , loc "Florida" "Deerfield Beach" (latlon 26.318056 -80.099724) eastern
        , loc "Oregon" "Medford" (latlon 42.33194 -122.86167) pacific
        , loc "California" "Chino Hills" (latlon 33.97 -117.75) pacific
        , loc "Michigan" "Kalamazoo" (latlon 42.29 -85.59) eastern
        , loc "California" "Mountain View" (latlon 37.392776 -122.04195) pacific
        , loc "California" "San Ramon" (latlon 37.78 -121.97806) pacific
        , loc "Washington" "Kennewick" (latlon 46.203476 -119.15927) pacific
        , loc "California" "Upland" (latlon 34.0999 -117.647) pacific
        , loc "Arkansas" "Fayetteville" (latlon 36.07639 -94.160835) central
        , loc "Connecticut" "New Britain" (latlon 41.675 -72.787224) eastern
        , loc "Ohio" "Canton" (latlon 40.805 -81.37583) eastern
        , loc "California" "Union" (latlon 37.59639 -122.04833) pacific
        , loc "Wisconsin" "Appleton" (latlon 44.282223 -88.418335) central
        , loc "California" "Folsom" (latlon 38.672222 -121.157776) pacific
        , loc "Michigan" "Wyoming" (latlon 42.913612 -85.70556) eastern
        , loc "Louisiana" "Lake Charles" (latlon 30.214722 -93.20861) central
        , loc "Texas" "Baytown" (latlon 29.743889 -94.965836) central
        , loc "North Carolina" "Gastonia" (latlon 35.25528 -81.180275) eastern
        , loc "Michigan" "Southfield" (latlon 42.47333 -83.22195) eastern
        , loc "Texas" "Conroe" (latlon 30.3161 -95.4589) central
        , loc "Texas" "Pearsall" (latlon 28.8914 -99.095) central
        , loc "Delaware" "Wilmington" (latlon 39.748333 -75.55139) eastern
        , loc "Rhode Island" "Pawtucket" (latlon 41.875557 -71.376114) eastern
        , loc "Michigan" "Rochester Hills" (latlon 42.658054 -83.14972) eastern
        , loc "South Dakota" "Rapid" (latlon 44.076187 -103.228294) mountain
        , loc "Wisconsin" "Waukesha" (latlon 43.011665 -88.23167) central
        , loc "Minnesota" "Plymouth" (latlon 45.010555 -93.45556) central
        , loc "Texas" "Pharr" (latlon 26.206388 -98.18528) central
        , loc "California" "Pleasanton" (latlon 37.6725 -121.8825) pacific
        , loc "Arkansas" "Jonesboro" (latlon 35.833332 -90.7) central
        , loc "North Carolina" "Jacksonville" (latlon 34.759724 -77.40972) eastern
        , loc "California" "Milpitas" (latlon 37.434723 -121.895) pacific
        , loc "Indiana" "Muncie" (latlon 40.193333 -85.388054) eastern
        , loc "Montana" "Missoula" (latlon 46.8625 -114.011665) mountain
        , loc "Arkansas" "Springdale" (latlon 36.18139 -94.145836) central
        , loc "New Jersey" "Passaic" (latlon 40.8575 -74.12889) eastern
        , loc "California" "Lynwood" (latlon 33.92472 -118.20194) pacific
        , loc "Tennessee" "Franklin" (latlon 35.929165 -86.8575) central
        , loc "California" "Redlands" (latlon 34.05472 -117.1825) pacific
        , loc "Arizona" "Flagstaff" (latlon 35.199165 -111.63111) mountain
        , loc "California" "Turlock" (latlon 37.505833 -120.84889) pacific
        , loc "Iowa" "Waterloo" (latlon 42.492435 -92.34616) central
        , loc "California" "Perris" (latlon 33.7825 -117.228615) pacific
        , loc "Florida" "Boynton Beach" (latlon 26.528055 -80.076385) eastern
        , loc "Iowa" "Iowa" (latlon 41.655834 -91.525) central
        ]
  where loc = Location "USA"

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
