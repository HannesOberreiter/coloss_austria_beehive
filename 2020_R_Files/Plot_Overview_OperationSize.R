##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Compare anonyme vs not-anonyme (= without given contact adress) ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 12, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim", "chi")
    )

CACHE.M     <- F_EXTRACT_N( D.FULL, "contact", "Anonymität" )
CACHE.BIND  <- F_GLM_FACTOR( D.FULL, "contact", D.FULL$contact, TRUE )
CACHE.BIND  <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS   <- rbind( D.FACTORS, CACHE.BIND )

# cleanup
rm(CACHE.M, CACHE.BIND)

D.FACTORS.PLOT <- D.FACTORS
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == 1 ] <- "Nicht-Anonyme Teilnahme"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == 0 ] <- "Anonyme Teilnahme"

p <- F_SINGLE_PLOT(D.FACTORS.PLOT)

ggsave("./img/plot_anonymity.pdf", p, width = 5, height = 4, units = "in")

