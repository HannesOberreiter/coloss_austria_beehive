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

CACHE.M     <- F_EXTRACT_N( D.FULL, "contact", "Anonymität" )
CACHE.BIND  <- F_GLM_FACTOR( D.FULL, "contact", D.FULL$contact, TRUE )
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )

# cleanup
rm(CACHE.M, CACHE.BIND)

D.FACTORS.PLOT <- D.FACTORS
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == 1 ] <- "Nicht-Anonyme Teilnahme"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == 0 ] <- "Anonyme Teilnahme"

D.SIGN <- F_CHISTAR_DF(D.FACTORS.PLOT, "Anonyme Teilnahme", "Nicht-Anonyme Teilnahme")

p <- F_SINGLE_PLOT(D.FACTORS.PLOT, D.SIGN)

ggsave("./img/plot_anonymity.pdf", p, width = 5, height = 4, units = "in")

