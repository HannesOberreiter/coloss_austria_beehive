##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Thymol ###########

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
#### Multiple Months ####
V.LABELS <- c("0 Monate", "1 Monat", "> 1 Monate")
# Remove if people did treatment but did not answer the months (otherwise our 0 months will be wrong!)
D.SUP <- D.FULL[!is.na(D.FULL$T_amount) | !(D.FULL$T_amount == 0 & D.FULL$varroa_treated == "Ja"),]
# generate grouping
D.SUP$groups <- V.LABELS[1]
# only use people which did only this treatment
#D.SUP$groups[D.SUP$T_thymol_total12 == 1 & D.SUP$T_amount == 1] <- V.LABELS[2]
#D.SUP$groups[D.SUP$T_thymol_total12 > 1 & (D.SUP$T_amount == D.SUP$T_thymol_total12)] <- V.LABELS[3]
D.SUP$groups[D.SUP$T_thymol_total12 == 1] <- V.LABELS[2]
D.SUP$groups[D.SUP$T_thymol_total12 > 1] <- V.LABELS[3]

# calculate glm
CACHE.M    <- F_EXTRACT_N( D.SUP, "groups", "groups" )
CACHE.BIND <- F_GLM_FACTOR( D.SUP, "groups", get( "groups", pos = D.SUP), TRUE )
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )
# cleanup
rm(CACHE.BIND, CACHE.M)
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABELS)
D.SIGN <- F_CHISTAR_DF(D.FACTORS, "0 Monate", "1 Monat")
# plotting
p <- F_SINGLE_PLOT(D.FACTORS, D.SIGN)
D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)

ggsave("./img/plot_treatment_thymol_grouped.pdf", p, width = 5, height = 4, units = "in")
