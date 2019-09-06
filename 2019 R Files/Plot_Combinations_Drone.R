##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### COMBINATION DRONE PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

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

# negative logic, participants which did not 
CACHE.COMB.negative <- F_COMBINATION(D.FULL, ColComb1, 1, CacheList, ColComb1, 0)[1:2,]
CACHE.COMB.negative$ff <- "No"
CACHE.COMB1.negative <- F_COMBINATION(D.FULL, ColComb2, 2, CacheList, ColComb1,0)[1,]
CACHE.COMB1.negative$ff <- "No"

# positive logic, participants which did
CACHE.COMB <- F_COMBINATION(D.FULL, ColComb1, 1, CacheList, ColComb1, 1)
CACHE.COMB$ff <- "Yes"
CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb2, 2, CacheList, ColComb1)
CACHE.COMB1$ff <- "Yes"

CACHE.COMB.PLOT <- rbind(CACHE.COMB, CACHE.COMB1, CACHE.COMB1.negative)
#CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb3, 3, CacheList, ColComb1)
#CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB1)

# D.COMB <- D.COMB[D.COMB$n != 0, ]

xAxisTemp <- c("Only in spring", "Only in summer", "Spring and summer", "No drone brood removal")

p1 <- 
  ggplot( CACHE.COMB.PLOT, aes( x = xAxisTemp, y = middle )) +
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 1.0 ) + 
  geom_text( aes( x = xAxisTemp, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  xlab("") + ylab("Loss rate [%]") + 
  ggtitle("Drone brood removal") +
  theme_classic() + 
  theme(
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    limits = xAxisTemp
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

ggsave("./img/Plot_Drone_Removal.pdf", p1, width = 6, height = 3, units = "in")