{-# LANGUAGE NegativeLiterals #-}

module Fetcher.EPA.Cities
  ( cities
  , usStateAbbreviation
  ) where

import qualified Data.Map as M
import Data.Maybe

import Types.Location

cities :: [LocationCoordinates]
cities =
  [ loc "New York" "New York" $ latlon 40.67 -73.94
  , loc "California" "Los Angeles" $ latlon 34.05 -118.25
  , loc "Illinois" "Chicago" $ latlon 41.9 -87.65
  , loc "Texas" "Houston" $ latlon 29.762777 -95.38306
  , loc "Arizona" "Phoenix" $ latlon 33.52833 -112.076385
  , loc "Texas" "San Antonio" $ latlon 29.428612 -98.49333
  , loc "Texas" "Dallas" $ latlon 32.7825 -96.7975
  , loc "California" "San Jose" $ latlon 37.304165 -121.87278
  , loc "Texas" "Austin" $ latlon 30.3 -97.73333
  , loc "Indiana" "Indianapolis" $ latlon 39.767776 -86.15806
  , loc "Florida" "Jacksonville" $ latlon 30.316668 -81.65
  , loc "Texas" "Fort Worth" $ latlon 32.75722 -97.33305
  , loc "California" "San Francisco" $ latlon 37.766666 -122.433334
  , loc "Ohio" "Columbus" $ latlon 39.983334 -82.98333
  , loc "Kentucky" "Louisville" $ latlon 38.254166 -85.76028
  , loc "Michigan" "Detroit" $ latlon 42.331665 -83.0475
  , loc "Tennessee" "Nashville" $ latlon 36.165 -86.78389
  , loc "Massachusetts" "Boston" $ latlon 42.357777 -71.06167
  , loc "Colorado" "Denver" $ latlon 39.739166 -104.984726
  , loc "Texas" "El Paso" $ latlon 31.790277 -106.42333
  , loc "Tennessee" "Memphis" $ latlon 35.1175 -89.97111
  , loc "Maryland" "Baltimore" $ latlon 39.28639 -76.615
  , loc "Washington" "Seattle" $ latlon 47.6 -122.316666
  , loc "Oklahoma" "Oklahoma" $ latlon 35.4823 -97.5352
  , loc "Oregon" "Portland" $ latlon 45.516666 -122.666664
  , loc "New Mexico" "Albuquerque" $ latlon 35.116665 -106.61667
  , loc "California" "Fresno" $ latlon 36.781666 -119.79222
  , loc "Missouri" "Kansas" $ latlon 39.05 -94.583336
  , loc "Georgia" "Atlanta" $ latlon 33.756943 -84.390274
  , loc "North Carolina" "Raleigh" $ latlon 35.81889 -78.64472
  , loc "Arizona" "Mesa" $ latlon 33.415 -111.83139
  , loc "Colorado" "Colorado Springs" $ latlon 38.863335 -104.79195
  , loc "Ohio" "Cleveland" $ latlon 41.482224 -81.66972
  , loc "Oklahoma" "Tulsa" $ latlon 36.13139 -95.937225
  , loc "Kansas" "Wichita" $ latlon 37.68889 -97.33611
  , loc "Louisiana" "New Orleans" $ latlon 29.966667 -90.05
  , loc "Texas" "Arlington" $ latlon 32.705032 -97.12284
  , loc "Florida" "Tampa" $ latlon 27.970833 -82.46472
  , loc "California" "Bakersfield" $ latlon 35.405834 -119.01861
  , loc "California" "Anaheim" $ latlon 33.836113 -117.889725
  , loc "Colorado" "Aurora" $ latlon 39.695835 -104.80805
  , loc "California" "Santa Ana" $ latlon 33.740833 -117.881386
  , loc "Missouri" "St. Louis" $ latlon 38.616665 -90.2
  , loc "Texas" "Corpus Christi" $ latlon 27.742777 -97.40195
  , loc "California" "Riverside" $ latlon 33.948063 -117.396126
  , loc "Alaska" "Anchorage" $ latlon 61.218334 -149.89917
  , loc "Kentucky" "Lexington" $ latlon 38.029724 -84.49472
  , loc "California" "Stockton" $ latlon 37.975555 -121.300835
  , loc "Minnesota" "Saint Paul" $ latlon 44.944168 -93.09361
  , loc "New Jersey" "Newark" $ latlon 40.73528 -74.185
  , loc "Nebraska" "Lincoln" $ latlon 40.810555 -96.680275
  , loc "North Carolina" "Greensboro" $ latlon 36.08 -79.81944
  , loc "Texas" "Plano" $ latlon 33.05 -96.75
  , loc "Nevada" "Henderson" $ latlon 36.0292 -115.0253
  , loc "Indiana" "Fort Wayne" $ latlon 41.08045 -85.13915
  , loc "California" "Irvine" $ latlon 33.684166 -117.7925
  , loc "New Jersey" "Jersey" $ latlon 40.711388 -74.06472
  , loc "Texas" "Laredo" $ latlon 27.506111 -99.507225
  , loc "California" "Chula Vista" $ latlon 32.627777 -117.04806
  , loc "Texas" "Lubbock" $ latlon 33.566666 -101.88333
  , loc "Florida" "Orlando" $ latlon 28.533611 -81.386665
  , loc "Wisconsin" "Madison" $ latlon 43.074722 -89.384445
  , loc "North Carolina" "Winston-Salem" $ latlon 36.1025 -80.26056
  , loc "Arizona" "Chandler" $ latlon 33.303333 -111.84139
  , loc "Louisiana" "Baton Rouge" $ latlon 30.416666 -91.1
  , loc "North Carolina" "Durham" $ latlon 35.9875 -78.90722
  , loc "Texas" "Garland" $ latlon 32.906944 -96.635
  , loc "Arizona" "Glendale" $ latlon 33.583332 -112.2
  , loc "Nevada" "Reno" $ latlon 39.52722 -119.821945
  , loc "Florida" "Hialeah" $ latlon 25.860556 -80.29389
  , loc "California" "Fremont" $ latlon 37.543056 -121.98278
  , loc "California" "Santa Clarita" $ latlon 34.41639 -118.506386
  , loc "Arizona" "Scottsdale" $ latlon 33.493057 -111.92611
  , loc "Nevada" "North Las Vegas" $ latlon 36.22861 -115.14667
  , loc "Texas" "Irving" $ latlon 32.81167 -96.950836
  , loc "New York" "Rochester" $ latlon 43.165554 -77.61139
  , loc "Alabama" "Montgomery" $ latlon 32.361668 -86.27917
  , loc "Idaho" "Boise" $ latlon 43.61361 -116.23778
  , loc "California" "Oxnard" $ latlon 34.191387 -119.1825
  , loc "North Carolina" "Fayetteville" $ latlon 35.066666 -78.9175
  , loc "Iowa" "Des Moines" $ latlon 41.59083 -93.620834
  , loc "California" "Fontana" $ latlon 34.1 -117.46667
  , loc "California" "Modesto" $ latlon 37.66139 -120.994446
  , loc "Georgia" "Columbus" $ latlon 32.49222 -84.94028
  , loc "Louisiana" "Shreveport" $ latlon 32.50821 -93.762955
  , loc "Washington" "Tacoma" $ latlon 47.24139 -122.45944
  , loc "Illinois" "Aurora" $ latlon 41.76 -88.298615
  , loc "Georgia" "Augusta" $ latlon 33.47 -81.975
  , loc "Alabama" "Mobile" $ latlon 30.727669 -88.05267
  , loc "California" "Moreno Valley" $ latlon 33.943054 -117.22833
  , loc "California" "Glendale" $ latlon 34.170834 -118.25
  , loc "Texas" "Amarillo" $ latlon 35.199165 -101.845276
  , loc "California" "Huntington Beach" $ latlon 33.69278 -117.999725
  , loc "Utah" "Salt Lake" $ latlon 40.75 -111.88333
  , loc "Texas" "Brownsville" $ latlon 25.930277 -97.48444
  , loc "Massachusetts" "Worcester" $ latlon 42.266666 -71.8
  , loc "Florida" "Tallahassee" $ latlon 30.438736 -84.28063
  , loc "Alabama" "Huntsville" $ latlon 34.71361 -86.58611
  , loc "Tennessee" "Knoxville" $ latlon 35.966667 -83.95
  , loc "Texas" "Grand Prairie" $ latlon 32.71528 -97.016945
  , loc "California" "Oceanside" $ latlon 33.211666 -117.325836
  , loc "Mississippi" "Jackson" $ latlon 32.29889 -90.18472
  , loc "Kansas" "Overland Park" $ latlon 38.94007 -94.680695
  , loc "Tennessee" "Chattanooga" $ latlon 35.045555 -85.26722
  , loc "Florida" "Fort Lauderdale" $ latlon 26.135834 -80.141945
  , loc "California" "Garden Grove" $ latlon 33.77889 -117.96028
  , loc "California" "Santa Rosa" $ latlon 38.448612 -122.70472
  , loc "California" "Rancho Cucamonga" $ latlon 34.123333 -117.579445
  , loc "South Dakota" "Sioux Falls" $ latlon 43.53639 -96.73167
  , loc "Florida" "Port St. Lucie" $ latlon 27.275833 -80.355
  , loc "Washington" "Vancouver" $ latlon 45.63361 -122.602776
  , loc "Arizona" "Tempe" $ latlon 33.429443 -111.943054
  , loc "California" "Corona" $ latlon 33.866665 -117.566666
  , loc "Missouri" "Springfield" $ latlon 37.195 -93.28611
  , loc "California" "Lancaster" $ latlon 34.686943 -118.15417
  , loc "Texas" "McKinney" $ latlon 33.2 -96.63333
  , loc "Florida" "Pembroke Pines" $ latlon 26.0125 -80.313614
  , loc "Oregon" "Salem" $ latlon 44.930832 -123.02889
  , loc "Florida" "Cape Coral" $ latlon 26.633333 -81.98333
  , loc "Arizona" "Peoria" $ latlon 33.5825 -112.23861
  , loc "Georgia" "Macon" $ latlon 32.83583 -83.64639
  , loc "Massachusetts" "Springfield" $ latlon 42.11241 -72.547455
  , loc "California" "Elk Grove" $ latlon 38.43833 -121.38194
  , loc "California" "Palmdale" $ latlon 34.581112 -118.100555
  , loc "California" "Salinas" $ latlon 36.677776 -121.655556
  , loc "California" "Hayward" $ latlon 37.668888 -122.08083
  , loc "California" "Pomona" $ latlon 34.060833 -117.75584
  , loc "Texas" "Pasadena" $ latlon 29.676111 -95.17389
  , loc "Virginia" "Alexandria" $ latlon 38.80472 -77.047226
  , loc "New Jersey" "Paterson, New Jersey" $ latlon 40.915554 -74.163055
  , loc "Kansas" "Kansas" $ latlon 39.106667 -94.67639
  , loc "California" "Torrance" $ latlon 33.83585 -118.34063
  , loc "Colorado" "Fort Collins" $ latlon 40.566666 -105.083336
  , loc "California" "Escondido" $ latlon 33.12472 -117.08083
  , loc "Texas" "Mesquite" $ latlon 32.78278 -96.60972
  , loc "Colorado" "Lakewood" $ latlon 39.70639 -105.102776
  , loc "Georgia" "Savannah" $ latlon 32.050835 -81.10389
  , loc "Illinois" "Naperville" $ latlon 41.748055 -88.16556
  , loc "Ohio" "Dayton" $ latlon 39.766666 -84.2
  , loc "Texas" "McAllen" $ latlon 26.216389 -98.23639
  , loc "California" "Sunnyvale" $ latlon 37.36889 -122.03694
  , loc "Texas" "Huntsville" $ latlon 30.723328 -95.55096
  , loc "Texas" "Killeen" $ latlon 31.105556 -97.72667
  , loc "California" "Orange" $ latlon 33.803055 -117.8325
  , loc "California" "Fullerton" $ latlon 33.88 -117.92861
  , loc "Michigan" "Warren" $ latlon 42.491943 -83.02389
  , loc "Tennessee" "Clarksville" $ latlon 36.52972 -87.35944
  , loc "Utah" "West Valley" $ latlon 40.689167 -111.99389
  , loc "California" "Visalia" $ latlon 36.317223 -119.33195
  , loc "Connecticut" "New Haven" $ latlon 41.308334 -72.925
  , loc "Michigan" "Sterling Heights" $ latlon 42.580276 -83.03028
  , loc "Florida" "Miramar" $ latlon 25.97889 -80.2825
  , loc "Florida" "Gainesville" $ latlon 29.665277 -82.33611
  , loc "Connecticut" "Stamford" $ latlon 41.09667 -73.55222
  , loc "Kansas" "Topeka" $ latlon 39.033333 -95.683334
  , loc "Texas" "Carrollton" $ latlon 32.99 -96.89333
  , loc "California" "Thousand Oaks" $ latlon 34.189445 -118.875
  , loc "Iowa" "Cedar Rapids" $ latlon 41.983334 -91.66861
  , loc "New Jersey" "Elizabeth" $ latlon 40.662224 -74.20917
  , loc "California" "Concord" $ latlon 37.978054 -122.03111
  , loc "Kansas" "Olathe" $ latlon 38.8808 -94.8031
  , loc "Connecticut" "Hartford" $ latlon 41.763332 -72.685
  , loc "Texas" "Waco" $ latlon 31.55139 -97.15583
  , loc "California" "Simi Valley" $ latlon 34.270832 -118.73917
  , loc "Texas" "Midland" $ latlon 32.005 -102.09917
  , loc "Washington" "Bellevue" $ latlon 47.5975 -122.159164
  , loc "Florida" "Coral Springs" $ latlon 26.270555 -80.25916
  , loc "Louisiana" "Lafayette" $ latlon 30.216667 -92.03333
  , loc "South Carolina" "Charleston" $ latlon 32.77611 -79.9325
  , loc "Florida" "Panama Beach" $ latlon 30.176666 -85.80556
  , loc "Colorado" "Thornton" $ latlon 39.9031 -104.954
  , loc "Texas" "Beaumont" $ latlon 30.08 -94.12666
  , loc "Arizona" "Surprise" $ latlon 33.629196 -112.3678
  , loc "Indiana" "Evansville" $ latlon 37.974724 -87.55583
  , loc "Texas" "Abilene" $ latlon 32.44639 -99.74555
  , loc "Texas" "Frisco" $ latlon 33.1414 -96.8131
  , loc "Utah" "Provo" $ latlon 40.244446 -111.660835
  , loc "California" "Vallejo" $ latlon 38.104088 -122.25664
  , loc "California" "Victorville" $ latlon 34.53611 -117.28833
  , loc "Illinois" "Peoria" $ latlon 40.720833 -89.60944
  , loc "Georgia" "Athens" $ latlon 33.955276 -83.38306
  , loc "Michigan" "Lansing" $ latlon 42.7335 -84.5467
  , loc "California" "El Monte" $ latlon 34.073334 -118.0275
  , loc "Texas" "Denton" $ latlon 33.216667 -97.13333
  , loc "California" "Berkeley" $ latlon 37.870277 -122.26806
  , loc "Michigan" "Grand Rapids" $ latlon 42.96125 -85.655716
  , loc "California" "Downey" $ latlon 33.938057 -118.13084
  , loc "Oklahoma" "Norman" $ latlon 35.22167 -97.418335
  , loc "Minnesota" "Rochester" $ latlon 44.0234 -92.46295
  , loc "Texas" "Odessa" $ latlon 31.863333 -102.365555
  , loc "Connecticut" "Waterbury" $ latlon 41.558334 -73.03694
  , loc "Illinois" "Elgin" $ latlon 42.039444 -88.28861
  , loc "California" "Costa Mesa" $ latlon 33.67 -117.91
  , loc "Massachusetts" "Lowell" $ latlon 42.639446 -71.31472
  , loc "Texas" "Round Rock" $ latlon 30.515 -97.6725
  , loc "California" "Inglewood" $ latlon 33.9575 -118.34611
  , loc "New Hampshire" "Manchester" $ latlon 42.990833 -71.46361
  , loc "Tennessee" "Murfreesboro" $ latlon 35.84614 -86.392075
  , loc "Texas" "Pearland" $ latlon 29.5544 -95.2958
  , loc "Missouri" "Columbia" $ latlon 38.948334 -92.333885
  , loc "Utah" "West Jordan" $ latlon 40.606388 -111.97611
  , loc "Florida" "Clearwater" $ latlon 27.973612 -82.76417
  , loc "Texas" "Tyler" $ latlon 32.35 -95.3
  , loc "Massachusetts" "Cambridge" $ latlon 42.375 -71.10611
  , loc "Florida" "Miami Gardens" $ latlon 25.942122 -80.26992
  , loc "Colorado" "Pueblo" $ latlon 38.266945 -104.62028
  , loc "Colorado" "Arvada" $ latlon 39.802765 -105.08749
  , loc "California" "Ventura" $ latlon 34.275 -119.227776
  , loc "Colorado" "Westminster" $ latlon 39.836113 -105.037224
  , loc "California" "West Covina" $ latlon 34.056667 -117.91861
  , loc "North Dakota" "Fargo" $ latlon 46.877224 -96.789444
  , loc "California" "Norwalk" $ latlon 33.906944 -118.083336
  , loc "California" "Fairfield" $ latlon 38.25778 -122.05417
  , loc "California" "Roseville" $ latlon 38.7525 -121.289444
  , loc "Texas" "Wichita Falls" $ latlon 33.896946 -98.515
  , loc "North Carolina" "High Point" $ latlon 35.970554 -79.9975
  , loc "Wisconsin" "Green Bay" $ latlon 44.513332 -88.01583
  , loc "Florida" "West Palm Beach" $ latlon 26.709723 -80.06416
  , loc "California" "Richmond" $ latlon 37.935833 -122.34778
  , loc "California" "Murrieta" $ latlon 33.569443 -117.2025
  , loc "California" "Burbank" $ latlon 34.18028 -118.32833
  , loc "Florida" "Palm Bay" $ latlon 27.997923 -80.670006
  , loc "Washington" "Everett" $ latlon 47.963333 -122.200554
  , loc "Michigan" "Flint" $ latlon 43.01 -83.69
  , loc "California" "Antioch" $ latlon 38.005 -121.80583
  , loc "Georgia" "Sandy Springs" $ latlon 33.9375 -84.368614
  , loc "Indiana" "South Bend" $ latlon 41.6725 -86.25528
  , loc "California" "Daly" $ latlon 37.68639 -122.46833
  , loc "Florida" "Lakeland" $ latlon 28.041111 -81.958885
  , loc "Colorado" "Centennial" $ latlon 39.59639 -104.84389
  , loc "New York" "Albany" $ latlon 42.65 -73.76667
  , loc "California" "Temecula" $ latlon 33.503334 -117.12361
  , loc "Texas" "College Station" $ latlon 30.601389 -96.314445
  , loc "Florida" "Pompano Beach" $ latlon 26.234722 -80.12556
  , loc "California" "Santa Maria" $ latlon 34.95139 -120.433334
  , loc "California" "El Cajon" $ latlon 32.798332 -116.96
  , loc "Texas" "Richardson" $ latlon 32.965557 -96.715836
  , loc "Wisconsin" "Kenosha" $ latlon 42.582222 -87.84556
  , loc "Texas" "Allen" $ latlon 33.099724 -96.663055
  , loc "California" "Rialto" $ latlon 34.11139 -117.3825
  , loc "Oklahoma" "Broken Arrow" $ latlon 36.03639 -95.78361
  , loc "California" "Jurupa Valley" $ latlon 33.9994 -117.475
  , loc "Michigan" "Dearborn" $ latlon 42.31133 -83.21348
  , loc "New Mexico" "Las Cruces" $ latlon 32.31972 -106.765274
  , loc "South Carolina" "North Charleston" $ latlon 32.885277 -80.016945
  , loc "Colorado" "Boulder" $ latlon 40.019444 -105.29278
  , loc "California" "San Mateo" $ latlon 37.554165 -122.31306
  , loc "Michigan" "Livonia" $ latlon 42.39722 -83.37361
  , loc "Michigan" "Livonia" $ latlon 42.40111 -83.37111
  , loc "California" "Compton" $ latlon 33.896667 -118.225
  , loc "California" "Clovis" $ latlon 36.82528 -119.70306
  , loc "Texas" "Lewisville" $ latlon 33.0383 -97.0061
  , loc "Massachusetts" "New Bedford" $ latlon 41.636112 -70.93472
  , loc "Georgia" "Roswell" $ latlon 34.0339 -84.3442
  , loc "California" "Vista" $ latlon 33.19361 -117.24111
  , loc "Massachusetts" "Brockton" $ latlon 42.083332 -71.01889
  , loc "California" "Mission Viejo" $ latlon 33.612778 -117.65611
  , loc "Texas" "San Angelo" $ latlon 31.45 -100.45
  , loc "Arizona" "Yuma" $ latlon 32.692223 -114.61528
  , loc "Colorado" "Greeley" $ latlon 40.416668 -104.7
  , loc "California" "Vacaville" $ latlon 38.35389 -121.97278
  , loc "Massachusetts" "Quincy" $ latlon 42.25 -71.0
  , loc "California" "Carson" $ latlon 33.83972 -118.25972
  , loc "Oregon" "Hillsboro" $ latlon 45.523056 -122.988335
  , loc "Missouri" "Lee's Summit" $ latlon 38.9225 -94.37417
  , loc "Washington" "Yakima" $ latlon 46.601944 -120.507774
  , loc "Washington" "Renton" $ latlon 47.486668 -122.195274
  , loc "Utah" "Orem" $ latlon 40.29875 -111.69649
  , loc "Alabama" "Tuscaloosa" $ latlon 33.206665 -87.53472
  , loc "Massachusetts" "Lynn" $ latlon 42.466667 -70.95
  , loc "California" "Hesperia" $ latlon 34.412777 -117.306114
  , loc "Oregon" "Beaverton" $ latlon 45.486946 -122.80361
  , loc "Washington" "Spokane Valley" $ latlon 47.6733 -117.239
  , loc "California" "Santa Monica" $ latlon 34.021946 -118.48139
  , loc "California" "Westminster" $ latlon 33.75139 -117.99389
  , loc "Utah" "Sandy" $ latlon 40.5725 -111.859726
  , loc "Massachusetts" "Fall River" $ latlon 41.70139 -71.155556
  , loc "Florida" "Sunrise" $ latlon 26.157223 -80.28611
  , loc "Connecticut" "Norwalk" $ latlon 41.093887 -73.41972
  , loc "Kansas" "Lawrence" $ latlon 38.97167 -95.235275
  , loc "New Mexico" "Rio Rancho" $ latlon 35.28611 -106.670555
  , loc "California" "Livermore" $ latlon 37.685555 -121.76417
  , loc "New Hampshire" "Nashua" $ latlon 42.7575 -71.46445
  , loc "Colorado" "Longmont" $ latlon 40.171665 -105.10917
  , loc "California" "Chico" $ latlon 39.74 -121.835556
  , loc "Indiana" "Carmel" $ latlon 39.966667 -86.1
  , loc "California" "Whittier" $ latlon 33.965557 -118.024445
  , loc "California" "Newport Beach" $ latlon 33.616665 -117.8975
  , loc "Florida" "Deltona" $ latlon 28.905 -81.21111
  , loc "California" "San Leandro" $ latlon 37.725 -122.15611
  , loc "New Jersey" "Trenton" $ latlon 40.22167 -74.75611
  , loc "Illinois" "Champaign" $ latlon 40.112778 -88.26111
  , loc "Alabama" "Hoover" $ latlon 33.386112 -86.80556
  , loc "Michigan" "Westland" $ latlon 42.324165 -83.400276
  , loc "Texas" "Sugar Land" $ latlon 29.599445 -95.61417
  , loc "Utah" "Ogden" $ latlon 41.22778 -111.96111
  , loc "California" "San Marcos" $ latlon 33.141945 -117.17028
  , loc "Texas" "League" $ latlon 29.499722 -95.08972
  , loc "Idaho" "Nampa" $ latlon 43.574722 -116.563614
  , loc "California" "Citrus Heights" $ latlon 38.69 -121.29
  , loc "California" "Alhambra" $ latlon 34.081944 -118.135
  , loc "California" "Tracy" $ latlon 37.738056 -121.43389
  , loc "Minnesota" "Bloomington" $ latlon 44.83361 -93.31
  , loc "Rhode Island" "Warwick" $ latlon 41.716667 -71.416664
  , loc "Connecticut" "Danbury" $ latlon 41.40222 -73.47111
  , loc "Ohio" "Parma" $ latlon 41.391666 -81.727776
  , loc "Oklahoma" "Edmond" $ latlon 35.65 -97.45
  , loc "Texas" "Edinburg" $ latlon 26.3042 -98.1639
  , loc "Michigan" "Troy" $ latlon 42.580276 -83.14306
  , loc "Washington" "Bellingham" $ latlon 48.75028 -122.475
  , loc "Indiana" "Hammond" $ latlon 41.6111 -87.4931
  , loc "California" "Buena Park" $ latlon 33.85611 -118.004166
  , loc "Texas" "Longview" $ latlon 32.5092 -94.7539
  , loc "Texas" "Mission" $ latlon 26.21139 -98.32111
  , loc "Rhode Island" "Cranston" $ latlon 41.783333 -71.441666
  , loc "California" "Lakewood" $ latlon 33.8475 -118.12
  , loc "Michigan" "Farmington Hills" $ latlon 42.48528 -83.376945
  , loc "Missouri" "O'Fallon" $ latlon 38.784443 -90.70805
  , loc "California" "Alameda" $ latlon 37.76389 -122.25694
  , loc "North Carolina" "Concord" $ latlon 35.4044 -80.6006
  , loc "California" "Merced" $ latlon 37.302223 -120.483055
  , loc "Illinois" "Bloomington" $ latlon 40.484165 -88.993614
  , loc "Wisconsin" "Racine" $ latlon 42.726112 -87.80583
  , loc "California" "Hemet" $ latlon 33.741943 -116.983055
  , loc "Massachusetts" "Lawrence" $ latlon 42.706944 -71.16361
  , loc "California" "Chino" $ latlon 34.017776 -117.69
  , loc "Florida" "Largo" $ latlon 27.909166 -82.7875
  , loc "California" "Menifee" $ latlon 33.678333 -117.16695
  , loc "Georgia" "Albany" $ latlon 31.582222 -84.16556
  , loc "New Jersey" "Camden" $ latlon 39.9368 -75.1066
  , loc "California" "Lake Forest" $ latlon 33.641666 -117.690834
  , loc "New York" "New Rochelle" $ latlon 40.92861 -73.784164
  , loc "California" "Napa" $ latlon 38.3 -122.3
  , loc "Utah" "St. George" $ latlon 37.095276 -113.57806
  , loc "California" "Redwood" $ latlon 37.482777 -122.236115
  , loc "Missouri" "St. Joseph" $ latlon 39.758057 -94.83667
  , loc "Georgia" "Johns Creek" $ latlon 34.0289 -84.1986
  , loc "Oregon" "Bend" $ latlon 44.05639 -121.30805
  , loc "California" "Bellflower" $ latlon 33.888054 -118.1275
  , loc "Arizona" "Avondale" $ latlon 33.433613 -112.349724
  , loc "Texas" "Bryan" $ latlon 30.6656 -96.3667
  , loc "Illinois" "Decatur" $ latlon 39.851665 -88.94417
  , loc "Florida" "Melbourne" $ latlon 28.116667 -80.63333
  , loc "California" "Indio" $ latlon 33.72 -116.23194
  , loc "Minnesota" "Brooklyn Park" $ latlon 45.094166 -93.35611
  , loc "California" "Tustin" $ latlon 33.739723 -117.813614
  , loc "Illinois" "Evanston" $ latlon 42.04114 -87.690056
  , loc "California" "Baldwin Park" $ latlon 34.08278 -117.97111
  , loc "Florida" "Palm Coast" $ latlon 29.538055 -81.223335
  , loc "Idaho" "Meridian" $ latlon 43.614166 -116.39889
  , loc "Florida" "Deerfield Beach" $ latlon 26.318056 -80.099724
  , loc "Oregon" "Medford" $ latlon 42.33194 -122.86167
  , loc "California" "Chino Hills" $ latlon 33.97 -117.75
  , loc "Michigan" "Kalamazoo" $ latlon 42.29 -85.59
  , loc "California" "Mountain View" $ latlon 37.392776 -122.04195
  , loc "California" "San Ramon" $ latlon 37.78 -121.97806
  , loc "Washington" "Kennewick" $ latlon 46.203476 -119.15927
  , loc "California" "Upland" $ latlon 34.0999 -117.647
  , loc "Arkansas" "Fayetteville" $ latlon 36.07639 -94.160835
  , loc "Connecticut" "New Britain" $ latlon 41.675 -72.787224
  , loc "Ohio" "Canton" $ latlon 40.805 -81.37583
  , loc "California" "Union" $ latlon 37.59639 -122.04833
  , loc "Wisconsin" "Appleton" $ latlon 44.282223 -88.418335
  , loc "California" "Folsom" $ latlon 38.672222 -121.157776
  , loc "Michigan" "Wyoming" $ latlon 42.913612 -85.70556
  , loc "Louisiana" "Lake Charles" $ latlon 30.214722 -93.20861
  , loc "Texas" "Baytown" $ latlon 29.743889 -94.965836
  , loc "North Carolina" "Gastonia" $ latlon 35.25528 -81.180275
  , loc "Michigan" "Southfield" $ latlon 42.47333 -83.22195
  , loc "Texas" "Conroe" $ latlon 30.3161 -95.4589
  , loc "Texas" "Pearsall" $ latlon 28.8914 -99.095
  , loc "Delaware" "Wilmington" $ latlon 39.748333 -75.55139
  , loc "Rhode Island" "Pawtucket" $ latlon 41.875557 -71.376114
  , loc "Michigan" "Rochester Hills" $ latlon 42.658054 -83.14972
  , loc "South Dakota" "Rapid" $ latlon 44.076187 -103.228294
  , loc "Wisconsin" "Waukesha" $ latlon 43.011665 -88.23167
  , loc "Minnesota" "Plymouth" $ latlon 45.010555 -93.45556
  , loc "Texas" "Pharr" $ latlon 26.206388 -98.18528
  , loc "California" "Pleasanton" $ latlon 37.6725 -121.8825
  , loc "Arkansas" "Jonesboro" $ latlon 35.833332 -90.7
  , loc "North Carolina" "Jacksonville" $ latlon 34.759724 -77.40972
  , loc "California" "Milpitas" $ latlon 37.434723 -121.895
  , loc "Indiana" "Muncie" $ latlon 40.193333 -85.388054
  , loc "Montana" "Missoula" $ latlon 46.8625 -114.011665
  , loc "Arkansas" "Springdale" $ latlon 36.18139 -94.145836
  , loc "New Jersey" "Passaic" $ latlon 40.8575 -74.12889
  , loc "California" "Lynwood" $ latlon 33.92472 -118.20194
  , loc "Tennessee" "Franklin" $ latlon 35.929165 -86.8575
  , loc "California" "Redlands" $ latlon 34.05472 -117.1825
  , loc "Arizona" "Flagstaff" $ latlon 35.199165 -111.63111
  , loc "California" "Turlock" $ latlon 37.505833 -120.84889
  , loc "Iowa" "Waterloo" $ latlon 42.492435 -92.34616
  , loc "California" "Perris" $ latlon 33.7825 -117.228615
  , loc "Florida" "Boynton Beach" $ latlon 26.528055 -80.076385
  , loc "Iowa" "Iowa" $ latlon 41.655834 -91.525
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
