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
# You need to register here for username:
# https://www.geonames.org
# https://www.geonames.org/export/credits.html
options(geonamesUsername = "")

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### Start Code ####
# remove empty ones if there are any
D.CACHE <- D.FULL %>% drop_na( longitude, latitude )

# select start and end number of row
# please be careful with hard limit of API for altitude, max. 1_000 per hour and 10_000 per day
paste("Max rows in our dataframe: ", nrow(D.CACHE))
      
start = 10
end = 20

# create empty vector were we add our values
v <- character(end-start+1)

# Loop with given limits
i <- 1
for (row in start:end) {
  # fetch altitude from API
  x <- GNsrtm3(D.CACHE[row, "latitude"],D.CACHE[row, "longitude"])$srtm3
  print(x)
  # add to vector
  v[i] <- x
  i <- i+1
}

# get rowIds for row names in CSV File
rowIds <- D.CACHE$id[start:end]
# save to csv
write.csv( v, 'altitude.csv', row.names = rowIds )

  


