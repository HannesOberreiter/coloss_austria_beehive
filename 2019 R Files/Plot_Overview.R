##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OPTERATION FACTOR PLOT ###########

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_map.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
# Sequences used as Classes
K.SEQ <- c( seq( 0, 150, 10 ), 20000 )
L.SEQ <- seq( 0, 100, 10 )
# Labels
K.GROUPS <- c( "1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", "121-130", "131-140", "141-150", ">150")
L.GROUPS <- c( "0-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%" )
# Add Sequences to our DF
D.FULL$size_group <- cut( D.FULL$hives_winter, K.SEQ, label = K.GROUPS, include.lowest = TRUE, right = TRUE )
# Drop rows with no lost rate
D.FULL <- D.FULL %>% drop_na( lost_rate )
D.FULL$loss_group <- cut( D.FULL$lost_rate, L.SEQ, label = L.GROUPS, include.lowest = TRUE, right = TRUE )

# Create Plot DF
D.PLOT <- D.FULL %>%
  group_by( size_group ) %>%
  summarise(
    n = n(),
    np = format( round( (n() / nrow(D.FULL) * 100), 1), nsmall = 1 ),
    n_h = sum( hives_winter ),
    n_hp = format( round( ( sum( hives_winter ) / sum( D.FULL$hives_winter ) * 100 ), 1), nsmall = 1 )
  )

# Create Plot DF
D.PLOT2 <- D.FULL %>%
  group_by(loss_group) %>%
  summarise(
    n = n(),
    np = format( round( (n() / nrow(D.FULL) * 100), 1), nsmall = 1 )
  )

#### CREATE MAP CLUSTER ####
D.CACHE <- subset( D.FULL, select = c( "latitude", "longitude" ) )
D.CACHE <- F_MAP_CLUSTER( D.CACHE )

#### PLOTTING #####
p1 <- ggplot( data = D.PLOT ) +
  aes( x = size_group, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Operation size") + ylab("Number of beekeepers (n)") + 
  ggtitle("(A) Numbers of participates in given operation size groups") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 750, 50 ),
    limits = c( 0, 750 )
  )

p2 <- ggplot( data = D.PLOT ) +
  aes( x = size_group, y = n_h) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(n_hp, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Operation size") + ylab("Number of hives (n)") + 
  ggtitle("(B) Number of hives in given operation size groups") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 7000, 500 ),
    limits = c( 0, 7000 )
  )

p3 <- ggplot( data = D.PLOT2 ) +
  aes( x = loss_group, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Loss per Company [%]") + ylab("Number of beekeepers (n)") + 
  ggtitle("(C) Distribution of losses") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 800, 50 ),
    limits = c( 0, 800 )
  )

p4 <- ggplot() + 
  geom_polygon(data = MF_DISTRICTS, aes( x = MF_DISTRICTS$long, y = MF_DISTRICTS$lat, group = MF_DISTRICTS$group ), fill="white", color = "black", size = 0.2 ) + 
  geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "black", size = 0.6 ) + 
  geom_point(data = D.CACHE, aes(x = D.CACHE$longitude, y = D.CACHE$latitude, size = D.CACHE$n) ) + 

  #scale_fill_distiller( aesthetics = "colour", direction = 1, na.value = "white", limits = c(min(D.CACHE$n), max(D.CACHE$n))) +
  coord_fixed() +
  xlab( "" ) + ylab( "" ) + labs( colour = "Number of beekeepers (n)", size = "Number of beekeepers (n)" ) +
  #guides( size = "none" ) +
  scale_size_continuous(range = c(0.5, 3)) + 
  ggtitle("(D) Rough main winter apiary location of participating beekeepers") +
  theme_classic() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

gtitle = textGrob( "Distribution of survey data" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2 ), c( 3, 4 ) )
grid.arrange( p1, p2, p3, p4,
              top = gtitle, 
              layout_matrix = lay)
