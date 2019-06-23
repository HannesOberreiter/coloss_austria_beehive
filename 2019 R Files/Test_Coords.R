##############################
##############################
### Survey Bee Hive Losses ###
### (c) Hannes Oberreiter ####
##############################
##############################
# This code will check the source for wrong coords 
# (e.g. place is not in state it should be or outside of the map) 
# Coords in source are from a batch geo code tool (e.g. https://geocode.localfocus.nl/)

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_map.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### Start Code ####
# remove empty ones
D.CACHE <- D.FULL %>% drop_na( longitude, latitude )

# List of discticts
D.DISTRICT <- D.CACHE %>% group_by( Bezirk ) %>% summarize( n = n() )
# Drop "In mehr as einem Bezirk" because we cannot know which one it belongs to
D.DISTRICT <- subset(D.DISTRICT, Bezirk != "In mehr als einem Bezirk")

# If we want to check a single district use this line
# D.DISTRICT <- subset(D.DISTRICT, Bezirk == "Weiz")

# Create our dummy Dataframe
D.RESULT <- data.frame(
  longitude = as.double(), latitude = as.double(), id = as.character(), Bezirk = as.character()) 

# Loop through all Districts
for (cache.loop in D.DISTRICT$Bezirk){
  map.cache <- subset( MAP_DISTRICTS, name == cache.loop )
  data.cache <- subset( D.CACHE, Bezirk == cache.loop, select = c("longitude", "latitude", "id", "Bezirk"))
  data.cache <- data.cache %>% drop_na()
  data.cache <- transform( data.cache, longitude = as.numeric( longitude ))
  data.cache <- transform( data.cache, latitude = as.numeric( latitude ))
  # Transform into SpatialPolygon
  coordinates(data.cache) <- ~ longitude + latitude
  # Project data onto df
  proj4string(data.cache) <- proj4string(map.cache)
  result.loop <- data.cache[ is.na( over( data.cache, as( map.cache, "SpatialPolygons"))),]
  dataframe.loop <- as.data.frame( result.loop )
  D.RESULT <- rbind( D.RESULT, dataframe.loop ) 
}

# Show result in Log
D.RESULT

# Save result to csv file
write.csv( D.RESULT, file="Test_Coords.csv" )

# Plot the wrong coords on a map
ggplot( MF_DISTRICTS, aes(x = long, y = lat)) + 
  geom_polygon( aes( group = group ), fill = "white", color = "black", size = 0.2) + 
  geom_point( data = D.RESULT, aes( longitude, latitude ), size = 1, color = "red") +
  geom_text( data = D.RESULT, aes( longitude, latitude, label = id ), hjust = -0.1, size = 3, color = "red") +
  coord_fixed() +
  xlab("") + ylab("") + 
  theme_void() +
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )



