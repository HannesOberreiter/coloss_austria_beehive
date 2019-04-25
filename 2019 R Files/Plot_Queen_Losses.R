##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### QUEEN LOSSES PLOT ###########

# Import Header
source( "Partials_Header.r" )
# Import our Custom Functions
source( "Partials_Functions.r" )

#### Queen Problems #####
# get total amount and count the rows, so we can calculate the rate
TOTAL <- D.FULL %>% select( queen_problems ) %>% na.omit() %>% nrow()
# mutate to englisch before grouping
D.QP <- D.FULL %>% 
  mutate(queen_problems = replace(queen_problems, queen_problems == "Häufiger", "More often")) %>%
  mutate(queen_problems = replace(queen_problems, queen_problems == "Normal", "Normal")) %>%
  mutate(queen_problems = replace(queen_problems, queen_problems == "Seltener", "More rare")) %>%
  mutate(queen_problems = replace(queen_problems, queen_problems == "Weiß nicht", "Don't know")) %>%
  group_by( queen_problems ) %>%
  summarize(
    n = n(),
    rate = 
      as.numeric(
        format(
          round(
            (
              ( n() ) / TOTAL * 100 ), 1), nsmall = 2))
  ) %>% na.omit()

#### TOTAL Lost Queens #####
# Not needed but good to read the DF manually if everything is correct
D.STATES <- D.FULL %>%
  group_by(Bundesland) %>% 
  summarize(n_states = n(),
            hives_winter = sum( hives_winter ),
            lost_a = sum( lost_a )
  )
D.AUSTRIA <- D.FULL %>% 
  summarize(Bundesland = "Österreich",
            n_states = n(),
            hives_winter = sum( hives_winter ),
            lost_a = sum( lost_a )
  )
D.STATES <- rbind(D.AUSTRIA, D.STATES)

# Austria Data
GLM.AUSTRIA <- glm( 
  cbind( D.FULL$lost_a, D.FULL$hives_spring_queen ) ~ 1, 
  family = quasibinomial( link = "logit" ), 
  data = D.FULL, na.action = na.omit )
SUMMARY.AUSTRIA <- summary( GLM.AUSTRIA )
AUSTRIA.ODDS <- GLM.AUSTRIA$fitted.values[1] * 100
AUSTRIA.LOW <- inv.logit( coef( GLM.AUSTRIA) - qt( 0.975, df = GLM.AUSTRIA$df.residual ) * SUMMARY.AUSTRIA$coefficients[, 2] ) * 100
AUSTRIA.MAX <- inv.logit( coef( GLM.AUSTRIA) + qt( 0.975, df = GLM.AUSTRIA$df.residual ) * SUMMARY.AUSTRIA$coefficients[, 2] ) * 100
AUSTRIA.BIND <- cbind( lowerlim = AUSTRIA.LOW, middle = AUSTRIA.ODDS, upperlim = AUSTRIA.MAX )

# Create a dummy DF, as we can use the same function, we could also change the logic of the function but yeah who knows
D.CACHE <- D.FULL
D.CACHE$hives_lost_e <- D.CACHE$lost_a
D.CACHE$hives_spring_e <- D.CACHE$hives_spring_queen
CACHE.BIND <- F_GLM_FACTOR( D.CACHE, "Bundesland", D.CACHE$Bundesland )
CACHE.BIND <- rbind( AUSTRIA.BIND, CACHE.BIND )
D.STATES <- cbind( D.STATES, CACHE.BIND )

#### New Queens #####
# We need the full amount to calculate the percentage
D.FULL$queen_lost_rate <- D.FULL$lost_a / D.FULL$hives_winter * 100
D.QN <- D.FULL %>% select( young_queens, hives_winter, queen_lost_rate ) %>% na.omit()
D.QN$young_rate <- 
  as.numeric(
    format(
      round(
        (
          ( D.QN$young_queens ) / D.QN$hives_winter * 100 ), 1), nsmall = 2))
# Got some supicious data, better remove them ...
D.QN <- D.QN %>% filter( young_rate <= 100 )

D.QN2 <- D.FULL %>% select( young_queens, hives_winter, lost_rate_e ) %>% na.omit()
D.QN2$young_rate <- 
  as.numeric(
    format(
      round(
        (
          ( D.QN2$young_queens ) / D.QN2$hives_winter * 100 ), 1), nsmall = 2))
# Got some supicious data, better remove them ...
D.QN2 <- D.QN2 %>% filter( young_rate <= 100 )

# Data for our Regression line
fit_reg <- lm(queen_lost_rate ~ young_rate, data = D.QN)
fit_reg2 <- lm(lost_rate_e ~ young_rate, data = D.QN2)

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
  xlab("") + ylab("Probability of queen loss [%]") + 
  ggtitle("(A) Losses due queen problems - Total & States") +
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
    breaks = seq( 0, 20, 1 )
    #limits = c( 0, 10 )
  )


p2 <- 
  ggplot( D.QP, aes( x = queen_problems, y = rate )) +
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(rate, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  #geom_point() +
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 1.0 )+ 
  xlab("") + ylab("Participates queen problems occurence  [%]") + 
  ggtitle("(B) Subjective queen problems in comparison to last year(s)") +
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
    labels = paste( D.QP$queen_problems,"\n ( n = ",D.QP$n, " )", sep="" )
    #limits = c( levels( D.STATES$Bundesland ))
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 55, 5 ),
    limits = c( 0, 55 )
  )


# Text for Linear Regression, dumped it outside for better handling
text_reg <- paste("Adj R2 = ",signif(summary(fit_reg)$adj.r.squared, 5),
              "Intercept =",signif(fit_reg$coef[[1]],5 ),
              " Slope =",signif(fit_reg$coef[[2]], 5),
              " P =",signif(summary(fit_reg)$coef[2,4], 5))
p3 <- 
  ggplot( D.QN, aes( x = queen_lost_rate, y = young_rate )) +
  geom_point() + 
  annotate("text", label = text_reg, x = 55, y = 5) +
  stat_smooth(method = "lm", col = "red") +
  xlab("Hives mortality due queen related problems [%]") + 
  ylab("Amount of young queens [%]") + 
  ggtitle("(C) Relation between young queens and queen related losses") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    #panel.grid.minor.y = element_line( colour = "grey" ),
    panel.grid.major.x = element_line( colour = "grey" )
    #panel.grid.minor.x = element_line( colour = "grey" )
  ) +
  scale_x_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 10 ),
    limits = c( 0, 100 )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 10 ),
    limits = c( 0, 100 )
  )

# Text for Linear Regression, dumped it outside for better handling
text_reg2 <- paste("Adj R2 = ",signif(summary(fit_reg2)$adj.r.squared, 5),
                  "Intercept =",signif(fit_reg2$coef[[1]],5 ),
                  " Slope =",signif(fit_reg2$coef[[2]], 5),
                  " P =",signif(summary(fit_reg2)$coef[2,4], 5))

p4 <- 
  ggplot( D.QN2, aes( x = lost_rate_e, y = young_rate )) +
  geom_point() + 
  annotate("text", label = text_reg2, x = 55, y = 5) +
  stat_smooth(method = "lm", col = "red") +
  xlab("Hives mortality without loss due elements [%]") + 
  ylab("Amount of young queens [%]") + 
  ggtitle("(D) Relation between young queens and beehive mortality") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    #panel.grid.minor.y = element_line( colour = "grey" ),
    panel.grid.major.x = element_line( colour = "grey" )
    #panel.grid.minor.x = element_line( colour = "grey" )
  ) +
  scale_x_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 10 ),
    limits = c( 0, 100 )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 10 ),
    limits = c( 0, 100 )
  )

gtitle = textGrob( "Queen related losses, Winter 2018/2019" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2 ), c(3, 4))
grid.arrange( p1, p2, p3, p4,
             top = gtitle, 
             layout_matrix = lay)
