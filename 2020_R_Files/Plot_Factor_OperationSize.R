##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Operation Size on Hive Mortality ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

D.FULL <- D.RAW

# Label and Ordering
V.LABEL <- c("1-50 Völker", "1-25 Völker", "26-50 Völker", "> 50 Völker")

# Create Column for different Operation Sizes, we split into 2 and 3 different groups here
D.FULL$operation_size2 <- ifelse(D.FULL$hives_winter < 51, V.LABEL[1], V.LABEL[4])
D.FULL$operation_size3 <- ifelse(D.FULL$hives_winter < 51, V.LABEL[3], V.LABEL[4])
D.FULL$operation_size3[D.FULL$hives_winter < 26] <- V.LABEL[2]

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
    )

CACHE.M <- F_EXTRACT_N( D.FULL, "operation_size2", "operation_size2" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "operation_size2", get( "operation_size2", pos = D.FULL ) )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

CACHE.M <- F_EXTRACT_N( D.FULL, "operation_size3", "operation_size3" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "operation_size3", get( "operation_size3", pos = D.FULL ) )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

rm(CACHE.M, CACHE.BIND)

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABEL)

#### PLOTTING #####
p2 <- F_SINGLE_PLOT(D.FACTORS[D.FACTORS$c == "operation_size2",])
p3 <- F_SINGLE_PLOT(D.FACTORS[D.FACTORS$c == "operation_size3",])

ggsave("./img/plot_factor_operationsize2.pdf", p2, width = 5, height = 4, units = "in")
ggsave("./img/plot_factor_operationsize3.pdf", p3, width = 5, height = 4, units = "in")
