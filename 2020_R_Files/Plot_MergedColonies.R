##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Compare colonies merged yes/no ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

CACHE.M     <- F_EXTRACT_N( D.FULL, "colonies_merged", "Colonies Merged" )
CACHE.BIND  <- F_GLM_FACTOR( D.FULL, "colonies_merged", D.FULL$colonies_merged, TRUE )
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )

# cleanup
rm(CACHE.M, CACHE.BIND)

#D.SIGN <- F_CHISTAR_DF(D.FACTORS, "Anonyme Teilnahme", "Nicht-Anonyme Teilnahme")

p <- F_SINGLE_PLOT(D.FACTORS)

ggsave("./img/plot_coloniesmerged.pdf", p, width = 5, height = 4, units = "in")

