##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Operation Size on Hive Mortality ###########

# Set Working directory (uses API of RStudio)
#SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
#setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

# Label and Ordering
V.LABEL <- c("1-50 Völker", "1-20 Völker", "21-50 Völker", "> 50 Völker")

# Create Column for different Operation Sizes, we split into 2 and 3 different groups here
D.FULL$operation_size2 <- ifelse(D.FULL$hives_winter < 51, V.LABEL[1], V.LABEL[4])
D.FULL$operation_size3 <- ifelse(D.FULL$hives_winter < 51, V.LABEL[3], V.LABEL[4])
D.FULL$operation_size3[D.FULL$hives_winter < 21] <- V.LABEL[2]

CACHE.M    <- F_EXTRACT_N( D.FULL, "operation_size2", "operation_size2" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "operation_size2", get( "operation_size2", pos = D.FULL ), TRUE)
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )

CACHE.M <- F_EXTRACT_N( D.FULL, "operation_size3", "operation_size3" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "operation_size3", get( "operation_size3", pos = D.FULL ), TRUE )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

rm(CACHE.M, CACHE.BIND)

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABEL)
D.SIGN2 <- F_CHISTAR_DF(D.FACTORS[D.FACTORS$c == "operation_size2",], "1-50 Völker", "> 50 Völker")
D.SIGN3 <- F_CHISTAR_DF(D.FACTORS[D.FACTORS$c == "operation_size3",], "1-20 Völker", "> 50 Völker")
D.SIGN4 <- F_CHISTAR_DF(D.FACTORS[D.FACTORS$c == "operation_size3",], "21-50 Völker", "> 50 Völker")
D.SIGN <- rbind(D.SIGN3, D.SIGN4)
#### PLOTTING #####
p2 <- F_SINGLE_PLOT(D.FACTORS[D.FACTORS$c == "operation_size2",], D.SIGN2, ptitle = "(A) Betriebsgröße: 1-50 und >50 Völker", pylim = 22)
p3 <- F_SINGLE_PLOT(D.FACTORS[D.FACTORS$c == "operation_size3",], D.SIGN, ptitle= "(B) Betriebsgröße: 1-20, 21-50 und >50 Völker", pylim = 22)

D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)

ggsave("./img/plot_factor_operationsize2.pdf", p2, width = 5, height = 4, units = "in")
ggsave("./img/plot_factor_operationsize3.pdf", p3, width = 5, height = 4, units = "in")

lay <- rbind( c( 1, 2 ) )
p1 <- arrangeGrob( p2, p3,layout_matrix = lay)
ggsave("./img/plot_factor_operationsize_combined.pdf", p1, width = 11, height = 4, units = "in")

