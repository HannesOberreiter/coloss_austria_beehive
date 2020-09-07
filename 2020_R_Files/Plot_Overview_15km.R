##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Question if all apiaries are in 15km radius ###########

# Set Working directory (uses API of RStudio)
#SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
#setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

D.FACTORS <- F_EXTRACT_N(D.FULL, "apiary_nearby", "apiary_nearby", FALSE)
colnames(D.FACTORS)[1] = 'val'

p <- F_HISTO_PLOT(D.FACTORS, "", "Teilnehmende Imkereien (n)", "" )
ggsave("./img/plot_overview_radius.pdf", p, width = 11, height = 8, units = "in")
