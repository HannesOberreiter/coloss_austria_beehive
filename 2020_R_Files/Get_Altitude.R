##############################
##############################
### Survey Bee Hive Losses ###
### (c) Hannes Oberreiter ####
##############################
##############################
# This code will generate a list of altitudes 
# uses open source access off geonames.org
# altitude from satellite images GNsrtm3

require(rjson)
require(geonames)
# Personal geonameUsername from Hannes Oberreiter, don't abuse it or it get closed
# https://www.geonames.org
# https://www.geonames.org/export/credits.html
options(geonamesUsername = "btree")

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### Start Code ####
D.FULL <- D.RAW
# remove empty ones if there are any
D.CACHE <- D.FULL %>% drop_na( longitude, latitude )

# select start and end number of row
# please be careful with hard limit of API for altitude, max. 1_000 per hour and 10_000 per day
paste("Max rows in our dataframe: ", nrow(D.CACHE))

# Start and End Row we want to get elevation
ROW.START = 1
ROW.END   = 5

# Create a Named Vector with IDS
V.IDS    <- D.CACHE$id[ROW.START:ROW.END]
V.VALUES <- setNames(rep(NA, length(V.IDS)), V.IDS)

# Loop with given limits
for (i in ROW.START:ROW.END) {
  L.ID        <- D.CACHE$id[i]
  L.ELEVATION <- GNsrtm3(lat = D.CACHE[i, "latitude"], lng = D.CACHE[i, "longitude"])$srtm3
  print(paste("ID:", L.ID, " Elevation: ", L.ELEVATION))
  V.VALUES[L.ID] <- L.ELEVATION
}
print("------RESULTS--------")
V.VALUES
print("---------------------")

# save to csv
write.csv( V.VALUES, 'altitude.csv' )

  


