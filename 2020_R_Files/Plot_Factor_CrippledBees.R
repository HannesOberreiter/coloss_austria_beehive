##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Crippled Bees observed ###########

# Set Working directory (uses API of RStudio)
#SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
#setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW
V.LABEL <- c("Häufig", "Wenig", "Überhaupt\nnicht", "Weiß\nnicht", "keine\nAngaben")

# Remove Newspaper entries they did not have this question in the short questionnaire
D.FULL <- D.FULL[D.FULL$submitted != "Zeitung",]

# Calculating Losses
D.FULL$crippled_bees[is.na(D.FULL$crippled_bees)] <- "keine Angaben"
D.FACTORS <- F_EXTRACT_N(D.FULL, "crippled_bees", "crippled_bees", FALSE)
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "crippled_bees", get( "crippled_bees", pos = D.FULL ), TRUE, FALSE)
D.FACTORS  <- cbind( D.FACTORS, CACHE.BIND )
D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)
D.FACTORS$ff <- str_replace_all(D.FACTORS$ff, fixed(" "), "\n")
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABEL)

D.SIGN <- F_CHISTAR_DF(D.FACTORS, "Häufig", "Überhaupt\nnicht")
D.SIGN$y <- D.SIGN$y * 1.06
D.SIGN <- rbind(D.SIGN, F_CHISTAR_DF(D.FACTORS, "Häufig", "Wenig"))

# Plotting
p <- F_SINGLE_PLOT(D.FACTORS, D.SIGN)
ggsave("./img/plot_factor_crippledbees.pdf", p, width = 5, height = 4, units = "in")
