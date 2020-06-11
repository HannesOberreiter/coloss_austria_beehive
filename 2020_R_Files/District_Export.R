### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# --- District Export ----

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Header.r" )
source( "Partials_Functions.r" )

# ----- START CODE -----
# RAW into our working dataframe
D.FULL <- D.RAW
D.FULL <- D.FULL[ D.FULL[, "district"] != "In mehr als einem Bezirk",  ]

D.FACTOR <- D.FULL %>% group_by(state, district) %>% summarize(
  n = n(),
  n_hives = sum(hives_winter),
  hive_lost_rate = as.numeric( format( round(
    ( sum( hives_lost_e ) / sum( hives_winter ) * 100 ), 2), nsmall = 2))
)

D.FACTOR$text <- paste("(",D.FACTOR$n,"; ",D.FACTOR$n_hives,")", sep = "")
D.FACTOR <- D.FACTOR[D.FACTOR$n >= 5,]

write.csv( D.FACTOR, file = paste("./", "District_Losses.csv", sep = "" ) )

