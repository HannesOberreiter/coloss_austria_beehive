##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Migrating VS Non-Migrating Beekeepers ###########

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

CACHE.M <- F_EXTRACT_N( D.FULL, "op_migratory_beekeeper", "migratory_beekeeper", FALSE)
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "op_migratory_beekeeper", get( "op_migratory_beekeeper", pos = D.FULL ), TRUE, FALSE)
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

rm(CACHE.M, CACHE.BIND)

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = c("Ja", "Nein", "Unsicher"))

#### PLOTTING #####
# only use > 30 answers for plotting
p <- F_SINGLE_PLOT(D.FACTORS[D.FACTORS$n >= 30,])

ggsave("./img/plot_factor_migration.pdf", p, width = 5, height = 4, units = "in")
