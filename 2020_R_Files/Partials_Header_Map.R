### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# Header File, if you use a map in your plot

# ---- MAP Libraries ----
library(rgeos)
library(stringr)
library(rgdal)
library(maptools)
library(maps)
#library(mapproj)
# Error Workaround
library(gpclib)
#if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )


# Folder and Name ned to be the same
SHAPEFILE.NAME.AUSTRIA <- "VGD_modf"

#---- Load MAP----
SHAPEFILE.FOLDER.AUSTRIA  <- gsub(" ", "", paste(SCRIPT.DIR, SHAPEFILE.NAME.AUSTRIA, sep = "/"), fixed = TRUE)

# ---- Read MAPS -----
MAP_AUSTRIA    <- readOGR( dsn = SHAPEFILE.FOLDER.AUSTRIA, layer = SHAPEFILE.NAME.AUSTRIA )
MF_STATES     <- fortify( MAP_AUSTRIA, region = "BL" )
MF_DISTRICTS  <- fortify( MAP_AUSTRIA, region = "PB" )

# ---- Remove Temporary -----
rm(SHAPEFILE.FOLDER.AUSTRIA, SHAPEFILE.NAME.AUSTRIA)

