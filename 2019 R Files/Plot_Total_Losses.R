##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OVERALL LOSSES PLOT ###########

# Import Header
source( "Partials_Header.r" )
# Import Map Header
source( "Partials_Header_Map.r" )
# Import our Custom Functions
source( "Partials_Functions.r" )

#### STATES Plot Matrix ####
D.STATES <- D.FULL %>% 
  group_by(Bundesland) %>% 
  summarize(
    n_states = n(),
    hives_winter = sum(hives_winter),
    lost_a = sum(lost_a),
    lost_b = sum(lost_b),
    lost_c = sum(lost_c)
  )

D.STATES$lost_rate = 
  as.numeric(
    format(
      round(
        (
          ( D.STATES$lost_a + D.STATES$lost_c ) / D.STATES$hives_winter * 100 ), 1), nsmall = 2))

D.STATES$lost_rate_elements = 
  as.numeric(
    format(
      round(
        (
          ( D.STATES$lost_a + D.STATES$lost_b + D.STATES$lost_c ) / D.STATES$hives_winter * 100 ), 1), nsmall = 2))


#### DISTRICTS Plot Matrix ####
# Create DISTRICT DF & calculate loss rates
D.DISTRICTS <- D.FULL %>% 
  group_by( Bezirk, Bundesland ) %>% 
  summarize( n = n(),
             hives_lost = sum( hives_lost_e ) / sum( hives_winter ) * 100
  )
# We only use data when there are aleast 6n
D.DISTRICTS <- subset( D.DISTRICTS, D.DISTRICTS$n > 5 )

#### ADD DATA TO MAP_D #####
MF_DISTRICTS$values = 0
MF_DISTRICTS = left_join( MF_DISTRICTS, D.DISTRICTS, by = c( "id" = "Bezirk" ), copy = TRUE )

#### Add Data to MAP_States ####
MF_STATES$values = 0
MF_STATES = left_join( MF_STATES, D.STATES, by = c( "id" = "Bundesland" ), copy = TRUE)

#### AUSTRIA DF #####
D.AUSTRIA <- D.FULL %>% 
  summarize(Bundesland = "Österreich",
            n_states = n(),
            hives_winter = sum(hives_winter),
            lost_a = sum(lost_a),
            lost_b = sum(lost_b),
            lost_c = sum(lost_c)
  )

D.AUSTRIA$lost_rate = 
  as.numeric(
    format(
      round(
        (
          ( D.AUSTRIA$lost_a + D.AUSTRIA$lost_c ) / D.AUSTRIA$hives_winter * 100 ), 1), nsmall = 2))

D.AUSTRIA$lost_rate_elements = 
  as.numeric(
    format(
      round(
        (
          ( D.AUSTRIA$lost_a + D.AUSTRIA$lost_b + D.AUSTRIA$lost_c ) / D.AUSTRIA$hives_winter * 100 ), 1), nsmall = 2))

D.STATES <- rbind(D.AUSTRIA, D.STATES)



#### GLM #####

# Austria Data
GLM.AUSTRIA <- glm( 
  cbind( D.FULL$hives_lost_e, D.FULL$hives_spring_e ) ~ 1, 
  family = quasibinomial( link = "logit" ), 
  data = D.FULL, na.action = na.omit )
SUMMARY.AUSTRIA <- summary( GLM.AUSTRIA )
AUSTRIA.ODDS <- GLM.AUSTRIA$fitted.values[1] * 100
AUSTRIA.LOW <- inv.logit( coef( GLM.AUSTRIA) - qt( 0.975, df = GLM.AUSTRIA$df.residual ) * SUMMARY.AUSTRIA$coefficients[, 2] ) * 100
AUSTRIA.MAX <- inv.logit( coef( GLM.AUSTRIA) + qt( 0.975, df = GLM.AUSTRIA$df.residual ) * SUMMARY.AUSTRIA$coefficients[, 2] ) * 100
AUSTRIA.BIND <- cbind( lowerlim = AUSTRIA.LOW, middle = AUSTRIA.ODDS, upperlim = AUSTRIA.MAX )


CACHE.BIND <- F_GLM_FACTOR( D.FULL, "Bundesland", D.FULL$Bundesland )
CACHE.BIND <- rbind( AUSTRIA.BIND, CACHE.BIND )
D.STATES <- cbind( D.STATES, CACHE.BIND )


#### PLOTTING #####
# Ordering
D.STATES <- transform(
  D.STATES, 
  Bundesland = factor(Bundesland,
  levels = c( "Österreich", "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien")))

p1 <- 
  ggplot( D.STATES, aes( x = Bundesland, y = middle )) +
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_point() +
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 1.0 )+ 
  xlab("") + ylab("Probability of loss [%]") + 
  ggtitle("(A) Austria Total & States") +
  #geom_text( aes( label = lost_rate ), angle = -90, vjust = 0, color = "black", size = 3 ) +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    labels = paste( D.STATES$Bundesland,"\n ( n = ",D.STATES$n_states, " )", sep="" ),
    limits = c( levels( D.STATES$Bundesland ))
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 30, 5 ),
    limits = c( 0, 30 )
  )

p2 <- ggplot() + 
  geom_polygon(data = MF_STATES, aes( x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group, fill = MF_STATES$lost_rate ), color = "black", size = 0.2 ) + 
  #geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "black", size = 0.6 ) + 
  #geom_text(data=map_d_text, aes(long, lat, label = loss_total_rate), size=3, color = "white") +
  coord_fixed() +
  scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white" ) +
  #scale_fill_distiller( palette = "DarkMint", direction = 1, na.value = "white") +
  xlab( "" ) + ylab( "" ) + labs( fill = "Prob. of loss [%]") +
  ggtitle("(B) Austria State - Median") +
  theme_classic() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

p3 <- ggplot() + 
  geom_polygon(data = MF_DISTRICTS, aes( x = MF_DISTRICTS$long, y = MF_DISTRICTS$lat, group = MF_DISTRICTS$group, fill = MF_DISTRICTS$hives_lost ), color = "black", size = 0.2 ) + 
  geom_path(data = MF_STATES, aes(x = MF_STATES$long, y = MF_STATES$lat, group = MF_STATES$group), color = "black", size = 0.6 ) + 
  #geom_text(data=map_d_text, aes(long, lat, label = loss_total_rate), size=3, color = "white") +
  coord_fixed() +
  scale_fill_continuous_sequential( palette = "Heat 2", aesthetics = "fill", na.value = "white" ) +
  # scale_fill_distiller( palette = "Greys", direction = 1, na.value = "grey") +
  xlab( "" ) + ylab( "" ) + labs( fill = "Prob. of loss [%]") +
  ggtitle("(C) Austria District - Median (white = n < 6)") +
  theme_classic() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )

gtitle = textGrob( "Probability of loss, Winter 2018/2019" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2 ), c( 1, 3) )
p1 <- arrangeGrob( p1, p2, p3,
             top = gtitle, 
             layout_matrix = lay)
# Save File
ggsave("./img/Plot_Total_Losses.pdf", p1, width = 11, height = 8, units = "in")
