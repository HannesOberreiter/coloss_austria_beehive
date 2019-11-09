##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OVERALL LOSSES PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
# Import Map Header
source( "Partials_Header_Map.r" )
# Import our Custom Functions
source( "Partials_Functions.r" )

#### Hive Production in season ####

# Remove NA rows, participants which did not answer spring hives question
D.FULL.PROD <- D.FULL[ !is.na( D.FULL$hives_production ),  ]
# Create dataframe with only needed columns
D.FULL.PROD <- select(D.FULL.PROD, hives_production, hives_winter, Bundesland)
# Rename column names, because our function can be used for different reasons, we use generic names
names( D.FULL.PROD ) <- c("a", "b", "f")
# First run for full austria sample
D.STATES.PROD <- F_BOOTSTRAP(D.FULL.PROD, "Österreich")
# Now loop over all factors (states)
state.list <- levels(factor(D.FULL.PROD$f))  
for( i in state.list) {
  temp.df <- F_BOOTSTRAP(D.FULL.PROD[D.FULL.PROD$f == i, ], i)
  D.STATES.PROD <- rbind(D.STATES.PROD, temp.df)
}
# We use the alpha later for plotting
D.STATES.PROD$alpha <- ifelse(D.STATES.PROD$Bundesland == "Österreich", 0.5, 0)

#### STATES Plot Matrix ####
D.STATES <- F_EXTRACT_N(D.FULL, "Bundesland", "STATES")

#### DISTRICTS Plot Matrix ####
# Remove "In more than one district rows"
D.FULL.DIS <- D.FULL[ D.FULL[, "Bezirk"] != "In mehr als einem Bezirk",  ]
# Create DISTRICT DF & calculate loss rates
D.DISTRICTS <- D.FULL.DIS %>% 
  group_by( Bezirk, Bundesland ) %>% 
  summarize( 
    n = n(),
    hives_lost = as.numeric( format( round(
      ( sum( hives_lost_e ) / sum( hives_winter ) * 100 ), 1), nsmall = 2))
  )

# GLM model by district
CACHE.DIS <- F_GLM_FACTOR( D.FULL.DIS, "Bezirk", D.FULL.DIS$Bezirk )
# Create DF from matrix
CACHE.DIS <- as_tibble(CACHE.DIS)
# Combine them, to check if order is correct you can check middle vs hive_lost cols
D.DISTRICTS <- bind_cols( D.DISTRICTS, CACHE.DIS )
# We only use data when there are aleast 6n
D.DISTRICTS <- D.DISTRICTS[ D.DISTRICTS[, "n" ] > 5, ]
# Write file to csv
write.csv( D.DISTRICTS, file = paste("./", "District_Losses.csv", sep = "" ) )

#### ADD DATA TO MAP_D #####
MF_DISTRICTS$values = 0
MF_DISTRICTS = left_join( MF_DISTRICTS, D.DISTRICTS, by = c( "id" = "Bezirk" ), copy = TRUE )

#### Add Data to MAP_States ####
MF_STATES$values = 0
MF_STATES = left_join( MF_STATES, D.STATES, by = c( "id" = "ff" ), copy = TRUE)

#### AUSTRIA DF #####
D.FULL.AUSTRIA <- D.FULL
D.FULL.AUSTRIA$Bundesland <- "Österreich"
D.AUSTRIA <- F_EXTRACT_N(D.FULL.AUSTRIA, "Bundesland", "STATES")
# We use the alpha later for plotting
D.AUSTRIA$alpha <- 0.5
D.STATES$alpha <- 0

D.STATES <- rbind(D.AUSTRIA, D.STATES)

#### GLM #####

# Austria Data
AUSTRIA.BIND <- F_GLM_SINGLE( D.FULL )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "Bundesland", D.FULL$Bundesland )
CACHE.BIND <- rbind( AUSTRIA.BIND, CACHE.BIND )
D.STATES <- cbind( D.STATES, CACHE.BIND )

#### PLOTTING #####
# Ordering
OrderVector <- c( "Österreich", "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien")
RenameVector <- c( "Austria", "Burgenland", "Carinthia", "Lower Austria", "Upper Austria", "Salzburg", "Styria", "Tyrol", "Vorarlberg", "Vienna")
D.STATES$ff <- factor( D.STATES$ff, levels = OrderVector )
D.STATES <- D.STATES[ order( factor( D.STATES$ff, levels = OrderVector )),]

# Workaround because ggplot uses for alpha 0 --> 0.1 
color_rule <- ifelse(D.STATES$alpha == 0, NA, "gray")

p1 <- 
  ggplot( D.STATES, aes( x = ff, y = middle )) +
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( aes(alpha = alpha), colour = "black", fill = color_rule, show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), alpha = 1, size = 1.0 )+ 
  xlab("") + ylab("Loss rate [%]") + 
  ggtitle("(A) Overall loss rate - Austria & States") +
  theme_classic() + 
  theme(
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    labels = paste( RenameVector,"\n ( n = ",D.STATES$n, " )", sep="" ),
    limits = c( levels( D.STATES$ff ))
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 30, 5 ),
    limits = c( 0, 30 )
  )

p2 <- ggplot() + 
  geom_polygon(data = MF_STATES, aes( x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group, fill = MF_STATES$hive_lost_rate ), color = "black", size = 0.2 ) + 
  coord_quickmap() +
  scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white", limits = c(9, 20), breaks = c(10, 12, 14, 16, 18, 20) ) +
  xlab( "" ) + ylab( "" ) + labs( fill = "Loss rate [%]") +
  ggtitle("(B) States - Loss rate") +
  theme_void() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

p3 <- ggplot() + 
  geom_polygon(data = MF_DISTRICTS, aes( x = MF_DISTRICTS$long, y = MF_DISTRICTS$lat, group = MF_DISTRICTS$group, fill = MF_DISTRICTS$hives_lost ), color = "black", size = 0.2 ) + 
  geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "black", size = 0.6 ) + 
  coord_quickmap() +
  scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white", limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) ) +
  xlab( "" ) + ylab( "" ) + labs( fill = "Loss rate [%]") +
  ggtitle("(C) Districts - Loss rate (white = n < 6)") +
  theme_void() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

p4 <- 
  ggplot( D.STATES.PROD, aes( x = Bundesland, y = mean )) +
  geom_bar( aes(alpha = alpha), colour = "black", fill = color_rule, show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lower.ci, ymax = upper.ci ), size = 0.5 )+ 
  xlab("") + ylab("Change rate [%]") + 
  ggtitle("Overall net increase in total colonies 2018") +
  theme_classic() + 
  theme(
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    labels = paste( D.STATES$ff,"\n ( n = ",D.STATES.PROD$n, " )", sep="" ),
    limits = c( levels( D.STATES$ff ))
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

#gtitle = textGrob( "Loss rate, Winter 2018/2019" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2 ), c( 1, 3) )
p <- arrangeGrob( p1, p2, p3,
             #top = gtitle, 
             layout_matrix = lay)
# Save File
#ggsave("./img/Plot_Total_Losses.pdf", p, width = 12, height = 8, units = "in")
ggsave("./img/Plot_Total_Losses.pdf", p, width = 10, height = 7, units = "in")

ggsave("./img/Plot_Total_Regeneration.pdf", p4, width = 5, height = 4, units = "in")

