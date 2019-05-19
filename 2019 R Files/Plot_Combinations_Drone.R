##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### COMBINATION DRONE PLOT ###########

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

#### DRONE BROOD REMOVAL #####

# List of Factors we want in our Plot
oList = list(
  c("A", "SPRING", "Drone brood removal"),
  c("B", "SUMMER", "Drone brood removal"),
  c("C", "WINTER", "Drone brood removal")
)

# Get column number with yn_* in it
ColComb1 <- grep("T_drone_totalyn_", colnames(D.FULL), fixed = TRUE)
# Calculate every possible combination
ColComb2 <- combn( ColComb1 , 2, simplify = FALSE )
ColComb3 <- combn( ColComb1 , 3, simplify = FALSE )

# Add to our oList with the names the ColNumber for better inserting later
CacheList <- data.frame(t(sapply(oList, c)))
CacheList <- cbind(CacheList, ColComb1)

CACHE.COMB <- F_COMBINATION(D.FULL, ColComb1, 1, CacheList, ColComb1)
CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb2, 2, CacheList, ColComb1)
CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB1)
CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb3, 3, CacheList, ColComb1)
CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB1)

# D.COMB <- D.COMB[D.COMB$n != 0, ]

xAxisTemp <- c("Only in spring", "Only in summer", "Spring and summer")

p1 <- 
  ggplot( CACHE.COMB, aes( x = xAxisTemp, y = middle )) +
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_point() +
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 1.0 ) + 
  geom_text( aes( x = xAxisTemp, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  xlab("") + ylab("Probability of loss [%]") + 
  ggtitle("Months of drone brood removal") +
  #geom_text( aes( label = lost_rate ), angle = -90, vjust = 0, color = "black", size = 3 ) +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    #labels = paste( D.STATES$Bundesland,"\n ( n = ",D.STATES$n_states, " )", sep="" ),
    limits = xAxisTemp
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 20, 5 ),
    limits = c( 0, 20 )
  )

ggsave("./img/Plot_Drone_Removal.pdf", p1, width = 4, height = 4, units = "in")