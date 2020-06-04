##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Loss Distrubtion Overview ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

# split into loss groups
V.SEQ <- seq( 0, 100, 10 )
V.GROUPS <- c( "0-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%" )
D.FULL$loss_group <- cut( D.FULL$lost_rate_e, V.SEQ, label = V.GROUPS, include.lowest = TRUE, right = TRUE )

# add zero group
D.FULL$loss_group <- as.character(D.FULL$loss_group)
D.FULL$loss_group[D.FULL$lost_rate_e == 0] <- "0%"
D.FULL$loss_group[D.FULL$loss_group == "0-10%"] <- ">0-10%"
D.FULL$loss_group <- as.factor(D.FULL$loss_group)
# Create Plot DF
D.PLOT <- D.FULL %>%
  group_by(loss_group) %>%
  summarise(
    n = n(),
    np = F_NUMBER_FORMAT(n() / nrow(D.FULL) * 100)
  )
V.ORDER <- c( "0%", ">0-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%" )
D.PLOT$loss_group <- factor( D.PLOT$loss_group, levels = V.ORDER)

# rename because funcion uses 'val' as x values
colnames(D.PLOT)[1] = 'val'
P.PLOT <- F_HISTO_PLOT(D.PLOT, "% Verlust/Imkerei", "Betroffene Imkereien [n]", "" )

ggsave("./img/plot_overview_loss_dist.pdf", P.PLOT, width = 6, height = 3.5, units = "in")

