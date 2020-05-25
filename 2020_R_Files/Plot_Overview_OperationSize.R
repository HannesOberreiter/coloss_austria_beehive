##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Operation Size of Participants ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

K.SEQ <- c( seq( 0, 150, 10 ), Inf )
K.GROUPS <- c( "1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", "121-130", "131-140", "141-150", ">150")

# Add Sequences to our DF
D.FULL$size_group <- cut( D.FULL$hives_winter, K.SEQ, label = K.GROUPS, include.lowest = TRUE, right = TRUE )

# Create Plot DF
D.SIZE.PLOT <- D.FULL %>%
  group_by( size_group ) %>%
  summarise(
    n = n(),
    np = F_NUMBER_FORMAT(n() / nrow(D.FULL) * 100)
  )

D.SIZE.PLOT.HIVES <- D.FULL %>%
  group_by( size_group ) %>%
  summarise(
    n = sum( hives_winter ),
    np = F_NUMBER_FORMAT( sum( hives_winter ) / sum( D.FULL$hives_winter ) * 100 )
  )

# rename because funcion uses 'val' as x values
colnames(D.SIZE.PLOT)[1] = 'val'
colnames(D.SIZE.PLOT.HIVES)[1] = 'val'

p1 <- F_HISTO_PLOT(D.SIZE.PLOT, "Völker / Imker", "Teilnehmende Imkereien (n)", "(A) Betriebsgröße der teilnehmenden Imker" )
p2 <- F_HISTO_PLOT(D.SIZE.PLOT.HIVES, "Völker / Imker", "Bienenvölker (n)", "(B) Völker/Betriebsgröße der teilnehmenden Imker", breaksize = 1000)

lay <- rbind( c( 1 ), c( 2 ) )
p <- arrangeGrob( p1, p2, layout_matrix = lay)
ggsave("./img/plot_overview_betrieb.pdf", p, width = 11, height = 8, units = "in")

