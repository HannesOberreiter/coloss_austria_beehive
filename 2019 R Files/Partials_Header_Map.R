##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

# Header File, if you use a map in your plot

#### MAP Libraries #####
library(rgeos)
library(stringr)
library(rgdal)
library(maptools)
library(maps)
### Error Workaround #####
library(gpclib)
#if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

SHAPEFILE.NAME.STATES <- "laender_95_geo" # Folder and Name need to be the same!
SHAPEFILE.NAME.DISCTRICTS <- "bezirke_95_geo" # Folder and Name need to be the same!

### Load MAP #####
SHAPEFILE.FOLDER.STATES <- gsub(" ", "", paste(SCRIPT.DIR, SHAPEFILE.NAME.STATES, sep = "/"), fixed = TRUE)
SHAPEFILE.FOLDER.DISTRICTS <- gsub(" ", "", paste(SCRIPT.DIR, SHAPEFILE.NAME.DISCTRICTS, sep = "/"), fixed = TRUE)

MAP_STATES <- readOGR( dsn = SHAPEFILE.FOLDER.STATES, layer = SHAPEFILE.NAME.STATES )
MF_STATES <- fortify( MAP_STATES, region = "name" )
MAP_DISTRICTS <- readOGR( dsn = SHAPEFILE.FOLDER.DISTRICTS, layer = SHAPEFILE.NAME.DISCTRICTS )
MF_DISTRICTS <- fortify( MAP_DISTRICTS, region = "name" )