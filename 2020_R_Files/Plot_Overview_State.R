### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

####### STATE Losses ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Header.r" )
source( "Partials_Functions.r" )

# ---- Start code ----
D.FULL <- D.RAW

D.STATES <- F_EXTRACT_N(D.RAW, "state", "STATES")
D.FULL.AUSTRIA <- D.RAW
D.FULL.AUSTRIA$State <- "Österreich"
D.AUSTRIA <- F_EXTRACT_N(D.FULL.AUSTRIA, "State", "STATES")
# We use the alpha later for plotting
D.AUSTRIA$alpha <- 'grey'
D.STATES$alpha <- 'white'

D.STATES <- rbind(D.AUSTRIA, D.STATES)

rm(D.AUSTRIA, D.FULL.AUSTRIA)

AUSTRIA.BIND <- F_GLM_SINGLE( D.FULL )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "state", D.FULL$state )
CACHE.BIND <- rbind( AUSTRIA.BIND, CACHE.BIND )
D.STATES <- cbind( D.STATES, CACHE.BIND )

rm(AUSTRIA.BIND, CACHE.BIND)

OrderVector <- c( "Österreich", "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien")
D.STATES$ff <- factor( D.STATES$ff, levels = OrderVector )
D.STATES <- D.STATES[ order( factor( D.STATES$ff, levels = OrderVector )),]
D.STATES$ff <- c("AUT", "Bgld.", "Ktn.", "NÖ", "OÖ", "Sbg.", "Stmk.", "T", "Vbg.", "W")

p <- F_SINGLE_PLOT(D.STATES, data_frame(empty=numeric()), D.STATES$alpha)

ggsave("./img/plot_overview_states.pdf", p, width = 6, height = 3.5, units = "in")

