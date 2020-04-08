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

D.FULL$op <- 'Internet'
D.FULL$op[grepl("P", D.FULL$id, fixed = TRUE)] <- 'Papier'
D.FULL$op[grepl("Z", D.FULL$id, fixed = TRUE)] <- 'Zeitung'

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 12, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim", "chi")
    )

CACHE.M     <- F_EXTRACT_N( D.FULL, "op", "Online vs. Paper vs. Zeitung" )
CACHE.BIND  <- F_GLM_FACTOR( D.FULL, "op", D.FULL$op, TRUE )
CACHE.BIND  <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS   <- rbind( D.FACTORS, CACHE.BIND )

# cleanup
rm(CACHE.M, CACHE.BIND)

D.FACTORS.PLOT <- D.FACTORS

p <- F_SINGLE_PLOT(D.FACTORS.PLOT)

ggsave("./img/plot_onlinevspaper.pdf", p, width = 5, height = 4, units = "in")

