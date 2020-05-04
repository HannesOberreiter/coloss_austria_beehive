##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### COMBINATION DRONE PLOT ###########

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

#### DRONE BROOD REMOVAL #####
# Remove illogical answers and newspaper as these questions are not included
D.FULL <- D.FULL[!(D.FULL$T_amount > 0 & D.FULL$varroa_treated == "Nein" | D.FULL$submitted == "Zeitung"),]

# List of Factors we want in our Plot
L.oList = list(
  c("A", "SPRING", "Drone brood removal", "Frühling"),
  c("B", "SUMMER", "Drone brood removal", "Sommer"),
  c("C", "WINTER", "Drone brood removal", "Herbst")
)

# Get column number with yn_* in it
V.ColComb1 <- grep("T_drone_totalyn_", colnames(D.FULL), fixed = TRUE)
# Calculate every possible combination
V.ColComb2 <- combn( V.ColComb1 , 2, simplify = FALSE )
V.ColComb3 <- combn( V.ColComb1 , 3, simplify = FALSE )

# Add to our oList with the names the ColNumber for better inserting later
L.CacheList <- data.frame(t(sapply(L.oList, c)))
L.CacheList <- cbind(L.CacheList, V.ColComb1)

L.CACHE = list()
# negative logic, participants which did not 
L.CACHE[[4]] <- F_COMBINATION(D.FULL, V.ColComb3, 3, L.CacheList, V.ColComb1, 0)
L.CACHE[[4]]$short <- "Kein \n entfernen der \n Drohnenbrut"
# positive combinations
L.CACHE[[3]] <- F_COMBINATION(D.FULL, V.ColComb3, 3, L.CacheList, V.ColComb1, 1)
L.CACHE[[2]] <- F_COMBINATION(D.FULL, V.ColComb2, 2, L.CacheList, V.ColComb1, 1)
L.CACHE[[1]] <- F_COMBINATION(D.FULL, V.ColComb1, 1, L.CacheList, V.ColComb1, 1)
L.CACHE[[1]]$short <- paste("Nur ", L.CACHE[[1]]$short, sep = "")
# automatically generate tibble from list
D.PLOTC <- bind_rows(L.CACHE)

V.LABELS <- D.PLOTC$short
V.LABELS <- str_replace_all(V.LABELS, "&", " & \n")
D.PLOTC$ff <- V.LABELS
D.PLOTC$ff <- factor( D.PLOTC$ff, levels = V.LABELS )
D.PLOTC$alpha <- 'white'
D.PLOTC$alpha[D.PLOTC$negative == 0] <- 'grey'

p <- F_SINGLE_PLOT(D.PLOTC, D.PLOTC$alpha)

ggsave("./img/plot_treatment_drone_combination.pdf", p, width = 6, height = 3.5, units = "in")

