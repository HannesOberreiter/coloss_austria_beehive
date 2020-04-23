##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### OLD FRAMES EXCHANGE FACTOR PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

D.FULL <- D.RAW

# we want the "NA" as no answers
D.FULL$op_new_frames[is.na(D.FULL$op_new_frames)] <- "keine Angaben"

# rename one factor for more conistense
D.FULL$op_new_frames[D.FULL$op_new_frames == "mehr als 50%"] <- "51-100%"

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 12, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim", "chi")
    )

CACHE.M <- F_EXTRACT_N( D.FULL, "op_new_frames", "op_new_frames" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "op_new_frames", get( "op_new_frames", pos = D.FULL ), TRUE )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

# Cleanup
rm(CACHE.M, CACHE.BIND)

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = c( "0%", "1-30%", "31-50%", "51-100%", "keine Angaben"))

#### PLOTTING #####
# only use > 30 answers for plotting
p <- F_SINGLE_PLOT(D.FACTORS[D.FACTORS$n >= 30,])

ggsave("./img/plot_factor_frames.pdf", p, width = 5, height = 4, units = "in")
