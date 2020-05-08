##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Compare online vs paper ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

CACHE.M     <- F_EXTRACT_N( D.FULL, "submitted", "Online vs. Paper vs. Zeitung" )
CACHE.BIND  <- F_GLM_FACTOR( D.FULL, "submitted", D.FULL$submitted, TRUE )
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )

# cleanup
rm(CACHE.M, CACHE.BIND)

D.FACTORS.PLOT <- D.FACTORS

p <- F_SINGLE_PLOT(D.FACTORS.PLOT)

ggsave("./img/plot_onlinevspaper.pdf", p, width = 5, height = 4, units = "in")

