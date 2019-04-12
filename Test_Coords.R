##############################
##############################
### Survey Bee Hive Losses ###
### (c) Hannes Oberreiter ####
##############################
##############################
# This code will check the source for wrong coords 
# (e.g. place is not in state it should be or outside of the map) 
# Coords in source are from a batch geo code tool (e.g. https://geocode.localfocus.nl/)

# Load Libraries
##############################
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Map Libraries
library(rgeos)
library(rgdal)
library(sp)

library(stringr)
library(maptools)
library(maps)
# Error Workaround, should be fixed in future version
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(SCRIPT.DIR)

# Variables and Names
##############################
# Our working files, will be imported from excel
FILE.NAME <- "Daten.xlsx"
SHEET.NAME <- "Winterverluste"
SHAPEFILE.NAME <- "bezirke_95_geo" # Folder and Name need to be the same!

# Load Excel File
data.full <- read_excel( FILE.NAME, sheet = SHEET.NAME, skip = 5 ) #Skip first 5 rows, as they are empty
# Load Maps
SHAPEFILE.FOLDER <- gsub(" ", "", paste(SCRIPT.DIR, SHAPEFILE.NAME, sep = "/"), fixed = TRUE)
map.disctricts <- readOGR( dsn = SHAPEFILE.FOLDER, layer = SHAPEFILE.NAME )
map.fortify <- fortify( map.disctricts, region = "name" )

# remove empty ones
data.full <- data.full %>% drop_na(longitude, latitude)

# List of discticts
cache.districts <- data.full %>% group_by(Bezirk) %>% summarize(n = n())

# If we want to check a single district use this line
#cache.districts <- subset(cache.districts, Bezirk == "Weiz")

# Create our dummy Dataframe
data.result <- data.frame(
  longitude = as.double(), latitude = as.double(), id = as.character(), Bezirk = as.character()) 

# Loop through all Districts
for (cache.loop in cache.districts$Bezirk){
  map.cache <- subset(map.disctricts, name == cache.loop)
  data.cache <- subset(data.full, Bezirk == cache.loop, select = c("longitude", "latitude", "id", "Bezirk"))
  data.cache <- data.cache %>% drop_na()
  data.cache <- transform(data.cache, longitude = as.numeric(longitude))
  data.cache <- transform(data.cache, latitude = as.numeric(latitude))
  # Transform into SpatialPolygon
  coordinates(data.cache) <- ~ longitude + latitude
  # Project data onto df
  proj4string(data.cache) <- proj4string(map.cache)
  result.loop <- data.cache[is.na(over(data.cache, as(map.cache, "SpatialPolygons"))),]
  dataframe.loop <- as.data.frame(result.loop)
  data.result <- rbind(data.result, dataframe.loop)
}

# Show result in Log
data.result
# Save result to csv file
write.csv(data.result, file="Test_Coord.csv")

# Plot the wrong coords on a map
ggplot(map.fortify, aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group), fill="white", color="black", size=0.2) + 
  geom_point(data=data.result, aes(longitude, latitude), size=1, color = "red") +
  geom_text(data=data.result, aes(longitude, latitude, label = id), hjust = -0.1, size=3, color = "red") +
  coord_fixed() +
  xlab("") + ylab("") + 
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank()
  )



