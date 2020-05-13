##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Compare Oxalic Mixture vs Pure Oxalic ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

# Calculate row sums to disciminate the two treament methods
V_MIX12  <- paste("(T_oxalic_trickle_mix_)\\S*0[1-9]|(T_oxalic_trickle_mix_)\\S*1[0-2]", sep = "")
V_PURE12 <- paste("(T_oxalic_trickle_pure_)\\S*0[1-9]|(T_oxalic_trickle_pure_)\\S*1[0-2]", sep = "")
V_MIX12  <- grepl(V_MIX12, colnames(D.FULL), fixed = FALSE)
V_PURE12 <- grepl(V_PURE12, colnames(D.FULL), fixed = FALSE)
# sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
D.FULL$T_oxalic_trickle_mix_sum = rowSums(D.FULL[, V_MIX12], na.rm = TRUE)
D.FULL$T_oxalic_trickle_pure_sum = rowSums(D.FULL[, V_PURE12], na.rm = TRUE)
# Generate Column for Faktors
V.LABELS <- c("Oxalsäure Mischung \n (Hiveclean/Bienenwohl/\nVarromed)", 
              "Oxalsäure Träufeln \n (oder Sprühen)",
              "Beides",
              "Behandlung  \n ohne Oxalsäure Träufeln \n oder Oxalsäure Mischung"
              )
D.FULL$oxalic_trickle_compare = NA
D.FULL$oxalic_trickle_compare[D.FULL$T_oxalic_trickle_mix_sum > 0 & 
                                D.FULL$T_oxalic_trickle_pure_sum == 0] = V.LABELS[1]
D.FULL$oxalic_trickle_compare[D.FULL$T_oxalic_trickle_pure_sum > 0 & 
                                D.FULL$T_oxalic_trickle_mix_sum == 0] = V.LABELS[2]
D.FULL$oxalic_trickle_compare[D.FULL$T_oxalic_trickle_pure_sum > 0 & 
                                D.FULL$T_oxalic_trickle_mix_sum > 0] = V.LABELS[3]
D.FULL$oxalic_trickle_compare[D.FULL$T_oxalic_trickle_mix_sum == 0 &
                                D.FULL$T_oxalic_trickle_pure_sum == 0 &
                                D.FULL$T_amount > 0] = V.LABELS[4]

# GLM
CACHE.M    <- F_EXTRACT_N( D.FULL, "oxalic_trickle_compare", "oxalic_trickle_compare" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "oxalic_trickle_compare", get( "oxalic_trickle_compare", pos = D.FULL), TRUE )
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABELS)

p <- F_SINGLE_PLOT(D.FACTORS)
ggsave("./img/plot_treatment_oxalmix.pdf", p, width = 7, height = 4, units = "in")

