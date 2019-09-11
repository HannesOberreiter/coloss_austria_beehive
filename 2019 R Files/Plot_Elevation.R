##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### Altitude LOSSES PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
# Import our Custom Functions
source( "Partials_Functions.r" )

D.CACHE <- D.FULL

L.SEQ <- c(seq( 0, 800, 200 ), Inf)
L.GROUPS <- c( "0-200m", "201-400m", "401-600m", "601-800m", ">800m" )
D.CACHE$altitude_group <- cut( D.CACHE$Altitude, L.SEQ, label = L.GROUPS, include.lowest = TRUE, right = TRUE )

D.PLOT_Q <- D.CACHE %>%
  group_by(altitude_group) %>%
  summarise(
    n = n(),
    np = F_NUMBER_FORMAT(n() / nrow(D.CACHE) * 100)
  )


CACHE.BIND <- F_GLM_FACTOR( D.CACHE, "altitude_group", D.CACHE$altitude_group )
D.PLOT_Q <- cbind( D.PLOT_Q, CACHE.BIND )

p <- ggplot( data = D.PLOT_Q ) +
  aes( x = altitude_group, y = middle ) + 
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = altitude_group, y = 1.5, label = paste("n = ", n )), color = "black", size = 3 ) +
  xlab("Elevation [m]") + ylab("Loss rate [%]") + 
  ggtitle("Loss rate to elevation of main wintering apiary") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )


# Save File
ggsave("./img/Plot_altitude.pdf", p, width = 6, height = 3, units = "in")



