##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Elevation LOSSES PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
# Import our Custom Functions
source( "Partials_Functions.r" )

#### Start Code ####
D.FULL <- D.RAW
V.SEQ <- c(seq( 0, 800, 200 ), Inf)
V.GROUPS <- c( "0-200m", "201-400m", "401-600m", "601-800m", ">800m" )
D.FULL$altitude_group <- cut(
  D.FULL$altitude, 
  V.SEQ, label = V.GROUPS,
  include.lowest = TRUE, right = TRUE )

D.FULL <- D.FULL[!is.na(D.FULL$altitude_group),]
D.FACTORS <- F_EXTRACT_N(D.FULL, "altitude_group", "altitude_group", TRUE)
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "altitude_group", D.FULL$altitude_group, TRUE)
D.FACTORS <- cbind( D.FACTORS, CACHE.BIND )

# cleanup
rm(CACHE.BIND)


p <- F_SINGLE_PLOT(D.FACTORS)
ggsave("./img/plot_elevation.pdf", p, width = 5, height = 4, units = "in")

