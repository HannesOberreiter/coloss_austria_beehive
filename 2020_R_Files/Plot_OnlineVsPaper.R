##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Compare online vs paper ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW
D.FULL$submitted[D.FULL$submitted == "Zeitung"] <- "Papier"

CACHE.M     <- F_EXTRACT_N( D.FULL, "submitted", "Online vs. Papier" )
CACHE.BIND  <- F_GLM_FACTOR( D.FULL, "submitted", D.FULL$submitted, TRUE )
D.FACTORS  <- cbind( CACHE.M, CACHE.BIND )

# cleanup
rm(CACHE.M, CACHE.BIND)

p <- F_SINGLE_PLOT(D.FACTORS)
D.ANNOTATION <- F_CHISTAR_DF(D.FACTORS, "Internet", "Papier", "c")
if(nrow(D.ANNOTATION)> 0){
  p1 <- p + geom_signif(data=D.ANNOTATION, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE)
}
p1
ggsave("./img/plot_onlinevspaper.pdf", p1, width = 5, height = 4, units = "in")

