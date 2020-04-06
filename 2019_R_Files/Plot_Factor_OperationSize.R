##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### Operation Size on Hive Mortality ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

# Create Column for different Operation Sizes, we split into 3 different groups here
D.FULL$operation_size <- ifelse(D.FULL$hives_winter < 51, "26-50 colonies", "> 50 colonies")
D.FULL$operation_size[D.FULL$hives_winter < 26] <- "1-25 colonies"

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
    )

CACHE.M <- F_EXTRACT_N( D.FULL, "operation_size", "operation_size" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "operation_size", get( "operation_size", pos = D.FULL ) )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, 
                             levels = c( "1-25 colonies", "26-50 colonies", "> 50 colonies"))

#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS ) +
  aes( x = ff, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  geom_point(size = 3) + 
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 2.5 ) +
  xlab("") + ylab("Loss rate [%]") + 
  #ggtitle("Loss rate by operation size") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
    ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limit = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 45, 5 )
  )

ggsave("./img/Plot_OperationSize.pdf", p1, width = 5, height = 4, units = "in")


