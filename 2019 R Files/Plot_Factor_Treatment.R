##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OPTERATION FACTOR PLOT ###########

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

# Just test for queen related mortality
#D.FULL$hives_lost_e <- D.FULL$lost_a
#D.FULL$hives_spring_e <- D.FULL$hives_spring_queen

#### SPRING ####

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
    )

# Loop through list and create for all factors CI
for( i in treatmentList){
  xn <- paste( i[2], "yn_spring", sep = "")
  # Check if there is only "no", means this treatment was not used at this time, next jumps to next iternation
  if( length( unique( get( xn, pos = D.FULL ))) == 1) next
  CACHE.M <- F_EXTRACT_N( D.FULL, xn, i[3] )
  
  # If there are not over 10 n we skip it
  testN <- CACHE.M$n[CACHE.M$ff == "1"]
  if( testN < 15) next
  
  CACHE.BIND <- F_GLM_FACTOR( D.FULL, xn, get( xn, pos = D.FULL ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Rename 1 and 0
D.FACTORS$ff <- ifelse(D.FACTORS$ff == 0, "No", "Yes")
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, 
                             levels = c( "Yes", "No", "Uncertain"))

p1 <- ggplot( data = D.FACTORS ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0.3, fill = "cornflowerblue", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "bottom", scales = "free_x", ncol = 5  ) +
  xlab("") + ylab("Probability of loss [%]") + 
  #ggtitle("Loss prob. by operational factors") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    #strip.background = element_blank(),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
    ) +
  scale_x_discrete(
    # labels = paste( D.FACTORS.PLOT$ff,"\n ( n = ",D.FACTORS.PLOT$n, " )", sep="" )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 35, 5 ),
    limits = c( 0, 35 )
  )

gtitle = textGrob( "Loss prob. by treatment method - SPRING (April, May)" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ) )
p1 <- arrangeGrob( p1,
              top = gtitle, 
              layout_matrix = lay)

ggsave("./img/P_FACTOR_Treatment_Spring.pdf", p1, width = 12, height = 6, units = "in")



#### SUMMER ####

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
  )

# Loop through list and create for all factors CI
for( i in treatmentList){
  xn <- paste( i[2], "yn_summer", sep = "")
  # Check if there is only "no", means this treatment was not used at this time, next jumps to next iternation
  if( length( unique( get( xn, pos = D.FULL ))) == 1) next
  CACHE.M <- F_EXTRACT_N( D.FULL, xn, i[3] )
  
  # If there are not over 10 n we skip it
  testN <- CACHE.M$n[CACHE.M$ff == "1"]
  if( testN < 15) next
  
  CACHE.BIND <- F_GLM_FACTOR( D.FULL, xn, get( xn, pos = D.FULL ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Rename 1 and 0
D.FACTORS$ff <- ifelse(D.FACTORS$ff == 0, "No", "Yes")
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, 
                        levels = c( "Yes", "No", "Uncertain"))

p1 <- ggplot( data = D.FACTORS ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0.3, fill = "forestgreen", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "bottom", scales = "free_x", ncol = 4  ) +
  xlab("") + ylab("Probability of loss [%]") + 
  #ggtitle("Loss prob. by operational factors") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    #strip.background = element_blank(),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    # labels = paste( D.FACTORS.PLOT$ff,"\n ( n = ",D.FACTORS.PLOT$n, " )", sep="" )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 35, 5 ),
    limits = c( 0, 35 )
  )

gtitle = textGrob( "Loss prob. by treatment method - SUMMER (June - October)" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ) )
p1 <- arrangeGrob( p1,
                   top = gtitle, 
                   layout_matrix = lay)

ggsave("./img/P_FACTOR_Treatment_Summer.pdf", p1, width = 10, height = 10, units = "in")



#### WINTER ####

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
  )

# Loop through list and create for all factors CI
for( i in treatmentList){
  xn <- paste( i[2], "yn_winter", sep = "")
  # Check if there is only "no", means this treatment was not used at this time, next jumps to next iternation
  if( length( unique( get( xn, pos = D.FULL ))) == 1) next
  CACHE.M <- F_EXTRACT_N( D.FULL, xn, i[3] )
  
  # If there are not over 10 n we skip it
  testN <- CACHE.M$n[CACHE.M$ff == "1"]
  if( testN < 15) next
  
  CACHE.BIND <- F_GLM_FACTOR( D.FULL, xn, get( xn, pos = D.FULL ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Rename 1 and 0
D.FACTORS$ff <- ifelse(D.FACTORS$ff == 0, "No", "Yes")
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, 
                        levels = c( "Yes", "No", "Uncertain"))

p1 <- ggplot( data = D.FACTORS ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0.3, fill = "grey13", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "bottom", scales = "free_x", ncol = 4  ) +
  xlab("") + ylab("Probability of loss [%]") + 
  #ggtitle("Loss prob. by operational factors") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    #strip.background = element_blank(),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    # labels = paste( D.FACTORS.PLOT$ff,"\n ( n = ",D.FACTORS.PLOT$n, " )", sep="" )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 35, 5 ),
    limits = c( 0, 35 )
  )

gtitle = textGrob( "Loss prob. by treatment method - WINTER (November - January)" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ) )
p1 <- arrangeGrob( p1,
                   top = gtitle, 
                   layout_matrix = lay)

ggsave("./img/P_FACTOR_Treatment_Winter.pdf", p1, width = 12, height = 6, units = "in")

