##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

####### FACTOR YIELD ###########

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_map.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

D.FULL <- D.RAW

# List of Factors we want in our Plot
oList = list(
  c("flow_brassica_napus", "(A) Raps"),
  c("flow_zea_mays", "(B) Mais"),
  c("flow_helianthus_annuus", "(C) Sonnenblume"),
  c("flow_late_catch_crop", "(D) Spätblüher"),
  c("flow_honeydew", "(E) Waldtracht"),
  c("flow_melezitose", "(F) Waldtracht mit Melezitose")
)

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 12, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim", "chi")
    )

# Loop through list and create for all factors CI
for( i in oList){
  temp.n <- get( i[1], pos = D.FULL )
  # remove "Unsicher" because AINOVA Test?
  #D.FULL_C <- subset( D.FULL, temp.n != "Unsicher" & D.FULL$migratory_beekeeper == "Nein" )
  #D.FULL_C <- subset( D.FULL, temp.n != "Unsicher" )
  D.FULL_C <- D.FULL
  CACHE.M <- F_EXTRACT_N( D.FULL_C, i[1], i[2] )
  CACHE.BIND <- F_GLM_FACTOR( D.FULL_C, i[1], get( i[1], pos = D.FULL_C ), TRUE )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Cleanup
rm(CACHE.BIND, CACHE.M)

# Translate Factors into english for plotting
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = c( "Ja", "Nein", "Unsicher"))

#### DISTRICTS Plot Matrix ####
# Create DISTRICT DF & calculate loss rates
D.DISTRICTS <- D.FULL %>% 
  group_by( district, state ) %>% 
  summarize( n = n(),
             hives_lost = F_NUMBER_FORMAT(sum( hives_lost_e ) / sum( hives_winter ) * 100)
  )
# We only use data when there are aleast 6n
D.DISTRICTS <- subset( D.DISTRICTS, D.DISTRICTS$n > 5 )

#### ADD DATA TO MAP_D #####
MF_DISTRICTS$values = 0
MF_DISTRICTS = left_join( MF_DISTRICTS, D.DISTRICTS, by = c( "id" = "district" ), copy = TRUE )

#### MAPS ####
D.PLOT_LIST <- list()
D.CACHE <- list()

count <- 0
for(i in oList){
  count <- count + 1

  label.fill = FALSE
  label.point = FALSE
  label.size = "none"
  legend.pos = "bottom"

  D.FULL$ff <- get( i[1], pos = D.FULL ) 
  D.CA <- subset( D.FULL, (D.FULL$ff == "Ja" & D.FULL$op_migratory_beekeeper == "Ja"), select = c( "latitude", "longitude" ) )
  D.CACHE[[i[1]]] <- F_MAP_CLUSTER(D.CA)
  # We need here aes_string!! inside functions or loops!
  # https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/aes_string
  p_cache <-
    ggplot() + 
    geom_polygon(data = MF_DISTRICTS, aes( x = MF_DISTRICTS$long, y = MF_DISTRICTS$lat, group = MF_DISTRICTS$group, fill = MF_DISTRICTS$hives_lost ), show.legend = label.fill, color = "Grey", size = 0.2 ) + 
    geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "Grey", size = 0.6 ) + 
    geom_point(data = D.CACHE[[i[1]]], aes_string(x = D.CACHE[[i[1]]]$longitude, y = D.CACHE[[i[1]]]$latitude, size = D.CACHE[[i[1]]]$n), color = "blue", fill = "black", stroke = 0.2, shape = 21, show.legend = label.point ) + 
    coord_quickmap() +
    # Info, when you want to join the legends for size and colour they need exact the same limits and breaks otherwise it wont work
    xlab( "" ) + ylab( "" ) + labs( colour = "Meldungen (n)", size = "Meldungen (n)", fill = "Verlustrate [%]" ) +
    scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white", limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) ) +
    scale_size_continuous( range = c(0.1, 3), breaks = c( 1, 5, 10, 15, 20 ), limits = c(0, 20), guide = label.size ) + 
    ggtitle(paste(i[2])) +
    theme_void() +
    theme(
      legend.position = legend.pos,
      legend.box = "vertical",
      plot.title = element_text(size = 10, hjust = 0.5), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      panel.grid.major = element_blank()
    )
  D.PLOT_LIST[[i[1]]] <- p_cache
}

#### DUMMY MAPS FOR LEGEND ####
D.CACHE.Legend <- list()
D.PLOT_LIST.Legend <- list()

count <- 0

for(i in oList){
  
  if (count == 3) {
    break
  }
  
  count <- count + 1
  
  label.fill <- ifelse(count == 1, TRUE, FALSE)
  label.point <- ifelse(count == 2, TRUE, FALSE)
  label.size <- ifelse(count == 2, "legend", "none")
  legen.pos <- 'bottom'
  
  D.FULL$ff <- get( i[1], pos = D.FULL ) 
  # only show on map if crop was present and not migratory beekeeprs
  D.CA <- subset( D.FULL, (D.FULL$ff == "Ja" & D.FULL$op_migratory_beekeeper == "Nein"), select = c( "latitude", "longitude" ) )
  D.CACHE.Legend[[i[1]]] <- F_MAP_CLUSTER(D.CA)
  # We need here aes_string!! inside functions or loops!
  # https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/aes_string
  p_cache <-
    ggplot() + 
    geom_polygon(data = MF_DISTRICTS, aes( x = MF_DISTRICTS$long, y = MF_DISTRICTS$lat, group = MF_DISTRICTS$group, fill = MF_DISTRICTS$hives_lost ), show.legend = label.fill, color = "Grey", size = 0.2 ) + 
    geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "Grey", size = 0.6 ) + 
    geom_point(data = D.CACHE[[i[1]]], aes_string(x = D.CACHE[[i[1]]]$longitude, y = D.CACHE[[i[1]]]$latitude, size = D.CACHE[[i[1]]]$n), color = "blue", fill = "black", stroke = 0.2, shape = 21, show.legend = label.point ) + 
    coord_quickmap() +
    # Info, when you want to join the legends for size and colour they need exact the same limits and breaks otherwise it wont work
    xlab( "" ) + ylab( "" ) + labs( colour = "Meldungen (n)", size = "Meldungen (n)", fill = "Verlustrate [%]" ) +
    scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white", limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) ) +
    scale_size_continuous( range = c(0.1, 3), breaks = c( 1, 5, 10, 15, 20 ), limits = c(0, 20), guide = label.size ) + 
    ggtitle(paste(i[2])) +
    theme_void() +
    theme(
      legend.position = legen.pos,
      legend.box = "vertical",
      plot.title = element_text(size = 10, hjust = 0.5), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      panel.grid.major = element_blank()
    )
  D.PLOT_LIST.Legend[[i[1]]] <- p_cache
}

# extract legends from our dummies
legend1 <- F_GG_LEGEND(D.PLOT_LIST.Legend[[1]])
legend2 <- F_GG_LEGEND(D.PLOT_LIST.Legend[[2]])


#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS ) +
  aes( x = ff, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_point(size = 3) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 2.5 ) +
  geom_text(data =  D.FACTORS[(D.FACTORS$chistar == 1 & D.FACTORS$ff == 'Ja'),], aes( x = ff, y = 17, label = "*"), angle = 0, vjust = 0, hjust = -5, color = "black", size = 8 ) +
  facet_wrap( ~ c, strip.position = "top", scales = "free_x", ncol = 3  ) +
  xlab("") + ylab("Verlustrate [%]") + 
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    #axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" ),
    panel.border = element_rect( fill = NA, linetype = "solid", colour = "black", size = 1 ),
    axis.line = element_blank()
    ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

# Cleanup
rm(count, i, label.fill, label.point, label.size, legen.pos, legend.pos, temp.n, oList, D.PLOT_LIST.Legend, D.CACHE.Legend, D.CACHE, D.CA)

# Layout for maps and Legend
lay <- rbind( 
  c( 2, 3 ), c( 2, 3 ), 
  c( 4, 5 ), c( 4, 5 ),
  c( 6, 7 ), c( 6, 7 ), 
  c(8, 9))
p2 <- arrangeGrob(D.PLOT_LIST[[1]], D.PLOT_LIST[[2]], D.PLOT_LIST[[3]], D.PLOT_LIST[[4]], D.PLOT_LIST[[5]], D.PLOT_LIST[[6]], legend1, legend2,
              #top = gtitle, 
              layout_matrix = lay)

# Save Files
ggsave("./img/plot_factor_yield.pdf", p1, width = 11, height = 6, units = "in")
ggsave("./img/plot_factor_yield_map.pdf", p2, width = 8, height = 6, units = "in")

