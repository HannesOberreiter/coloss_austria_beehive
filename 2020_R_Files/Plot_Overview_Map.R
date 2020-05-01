### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# --- Overview Map PLOT ----

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Header.r" )
source( "Partials_Header_map.r" )
source( "Partials_Functions.r" )

# ----- START CODE -----

# RAW into our working dataframe
D.FULL <- D.RAW

#### CREATE MAP CLUSTER ####
D.MAP.PLOT <- subset( D.FULL, select = c( "latitude", "longitude" ) )
D.MAP.PLOT <- F_MAP_CLUSTER( D.MAP.PLOT )


p <- ggplot() + 
  geom_polygon(data = MF_DISTRICTS, aes( x = long, y = lat, group = group ), fill="white", color = "black", size = 0.2 ) + 
  geom_path(data = MF_STATES, aes(x = long, y = lat, group = group), color = "black", size = 0.6 ) + 
  geom_point(data = D.MAP.PLOT, aes(x = longitude, y = latitude, size = n), color = "gray", fill = "darkblue", stroke = 0.3, shape = 21 ) + 
  coord_quickmap() +
  xlab( "" ) + ylab( "" ) + labs( colour = "Anzahl Imkereien (n)", size = "Anzahl Imkereien (n)") +
  scale_size_continuous(range = c(1, 3.5), breaks = c(1, 5, 10, 15)) + 
  ggtitle("") +
  theme_void() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )


ggsave("./img/plot_overview_map.pdf", p, width = 6, height = 3.5, units = "in")

#### CREATE DISTRICT LOSS MAP ####

#### DISTRICTS Plot Matrix ####
# Create DISTRICT DF & calculate loss rates
D.FULL.DIS <- D.FULL[ D.FULL[, "district"] != "In mehr als einem Bezirk",  ]
D.DISTRICTS <- D.FULL.DIS %>% 
  group_by( district, state ) %>% 
  summarize( n = n(),
             hives_lost = F_NUMBER_FORMAT(sum( hives_lost_e ) / sum( hives_winter ) * 100)
  )

# GLM model by district
CACHE.DIS <- F_GLM_FACTOR( D.FULL.DIS, "district", D.FULL.DIS$district )
# Create DF from matrix
CACHE.DIS <- as_tibble(CACHE.DIS)
# Combine them, to check if order is correct you can check middle vs hive_lost cols
D.DISTRICTS <- bind_cols( D.DISTRICTS, CACHE.DIS )
# We only use data when there are aleast 6n
D.DISTRICTS <- D.DISTRICTS[ D.DISTRICTS[, "n" ] > 5, ]
# Write file to csv
write.csv( D.DISTRICTS, file = paste("./", "District_Losses.csv", sep = "" ) )

# add data to map
MF_DISTRICTS$values = 0
MF_DISTRICTS = left_join( MF_DISTRICTS, D.DISTRICTS, by = c( "id" = "district" ), copy = TRUE )

# cleanup
rm(CACHE.DIS, D.FULL.DIS)

p2 <- ggplot() + 
  geom_polygon(data = MF_DISTRICTS, aes( x = long, y = lat, group = group, fill = hives_lost ), color = "black", size = 0.2 ) + 
  geom_path(data = MF_STATES, aes(x = long, y = lat, group = group), color = "black", size = 0.6 ) + 
  coord_quickmap() +
  scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white", limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) ) +
  xlab( "" ) + ylab( "" ) + labs( fill = "Verlustrate [%]") +
  #ggtitle("(C) Districts - Loss rate (white = n < 6)") +
  theme_void() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave("./img/plot_map_loss_district.pdf", p2, width = 6, height = 3.5, units = "in")



