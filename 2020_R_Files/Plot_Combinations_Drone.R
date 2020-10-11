##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### COMBINATION DRONE PLOT ###########

# Set Working directory (uses API of RStudio)
#SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
#setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW
# Remove participants which did not answer varroa_treated (this includes eg. beekeeping journal without given question)
D.FULL <- D.FULL[!is.na(D.FULL$varroa_treated),]

#### COMBINATION #####

D.FACTORS <- F_EXTRACT_N(D.FULL, "c_short_drone", "c_short_drone", FALSE)
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "c_short_drone", D.FULL$c_short_drone, TRUE, FALSE)
D.FACTORS <- cbind( D.FACTORS, CACHE.BIND )

V.LABELS = c("Nur Frühling", "Nur Sommer", "Frühling & \n Sommer", "Kein \n entfernen der \n Drohnenbrut")
D.FACTORS$ff <- c(V.LABELS[1], V.LABELS[3], V.LABELS[2], V.LABELS[4])
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABELS )

D.FACTORS$alpha <- 'white'
D.FACTORS$alpha[4] <- 'grey'
D.FACTORS$lower <- D.FACTORS$lowerlim
D.FACTORS$upper <- D.FACTORS$upperlim
D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)

p <- F_SINGLE_PLOT(D.FACTORS, data_frame(empty=numeric()), D.FACTORS$alpha)
p
ggsave("./img/plot_treatment_drone_combination.pdf", p, width = 6, height = 3.5, units = "in")

rm(L.oList, V.ColComb1, V.ColComb2, V.ColComb3, V.LABELS, L.CACHE, L.CacheList)

#### MULTIPLE MONTHS ####
V.LABELS <- c("0 Monate", "1-3 Monate", ">3 Monate")
D.SUP    <- D.FULL
# generate groups
D.SUP$g  <- V.LABELS[1]
D.SUP$g[D.SUP$T_drone_total > 0] <- V.LABELS[2]
D.SUP$g[D.SUP$T_drone_total > 3] <- V.LABELS[3]

# calculate GLM
CACHE.M <- F_EXTRACT_N( D.SUP, "g", "drone removal" )
CACHE.BIND <- F_GLM_FACTOR( D.SUP, "g", get( "g", pos = D.SUP), TRUE )
D.FACTORS <- cbind( CACHE.M, CACHE.BIND )
# cleanup
rm(CACHE.BIND, CACHE.M)
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABELS)
D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)
# Generate Plot
p2 <- F_SINGLE_PLOT(D.FACTORS)
ggsave("./img/plot_treatment_drone_grouped.pdf", p2, width = 5, height = 4, units = "in")
