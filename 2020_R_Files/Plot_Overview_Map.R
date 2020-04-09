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
  geom_polygon(data = MF_DISTRICTS, aes( x = MF_DISTRICTS$long, y = MF_DISTRICTS$lat, group = MF_DISTRICTS$group ), fill="white", color = "black", size = 0.2 ) + 
  geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "black", size = 0.6 ) + 
  geom_point(data = D.MAP.PLOT, aes(x = D.MAP.PLOT$longitude, y = D.MAP.PLOT$latitude, size = D.MAP.PLOT$n), color = "gray", fill = "darkblue", stroke = 0.3, shape = 21 ) + 
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
