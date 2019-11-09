##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### QUEEN LOSSES PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
# Import our Custom Functions
source( "Partials_Functions.r" )

#### Queen Problems #####

# get total amount and count the rows, so we can calculate the rate
TOTAL <- D.FULL %>% select( queen_problems ) %>% na.omit() %>% nrow()

# mutate to englisch before grouping
D.QP <- D.FULL %>% 
  group_by( queen_problems ) %>%
  summarize(
    rate = F_NUMBER_FORMAT( n() / TOTAL * 100 )
  ) %>% na.omit()

C.QP.N <- F_EXTRACT_N( D.FULL, "queen_problems", "Queen Problems" )
C.QP.N <- cbind(C.QP.N, D.QP)

C.QP.N$ff[ C.QP.N$ff == "Häufiger" ] <- "More often"
C.QP.N$ff[ C.QP.N$ff == "Normal" ] <- "Normal"
C.QP.N$ff[ C.QP.N$ff == "Seltener" ] <- "More rare"
C.QP.N$ff[ C.QP.N$ff == "Weiß nicht" ] <- "Don't know"

D.CACHE <- D.FULL
# queen related losses
D.CACHE$hives_lost_e <- D.CACHE$lost_a
C.QP.GLM <- F_GLM_FACTOR( D.CACHE, "queen_problems", D.CACHE$queen_problems )
C.QP <- cbind(C.QP.N, C.QP.GLM)

# Ordering factors and values so we get the right order in our plots because we use custom text we also need to order the values
OrderVector <- c( "Don't know", "More often", "Normal", "More rare")
C.QP$ff <- factor( C.QP$ff, levels = OrderVector )
C.QP <- C.QP[order(factor(C.QP$ff, levels = OrderVector )),]

# queen related losses
D.CACHE$hives_lost_e <- D.CACHE$lost_c
C.QP2.GLM <- F_GLM_FACTOR( D.CACHE, "queen_problems", D.CACHE$queen_problems )
C.QP2 <- cbind(C.QP.N, C.QP2.GLM)

# Ordering factors and values so we get the right order in our plots because we use custom text we also need to order the values
OrderVector <- c( "Don't know", "More often", "Normal", "More rare")
C.QP2$ff <- factor( C.QP2$ff, levels = OrderVector )
C.QP2 <- C.QP2[order(factor(C.QP2$ff, levels = OrderVector )),]

#### TOTAL Lost Queen Rate #####

# replace our columns with correct values for queen loss as we use our function
D.QUEEN.FULL <- D.FULL
D.QUEEN.FULL$hives_lost_e <- D.QUEEN.FULL$lost_a
D.QUEEN.FULL$hives_spring_e <- D.QUEEN.FULL$hives_spring_queen

# States
D.STATES <- F_EXTRACT_N(D.QUEEN.FULL, "Bundesland", "STATES")
CACHE.BIND <- F_GLM_FACTOR( D.QUEEN.FULL, "Bundesland", D.QUEEN.FULL$Bundesland )
D.STATES <- cbind(D.STATES, CACHE.BIND)

# Austria
D.FULL.AUSTRIA <- D.QUEEN.FULL
D.FULL.AUSTRIA$Bundesland <- "Österreich"
D.AUSTRIA <- F_EXTRACT_N(D.FULL.AUSTRIA, "Bundesland", "STATES")
AUSTRIA.BIND <- F_GLM_SINGLE( D.QUEEN.FULL )
D.AUSTRIA <- cbind(D.AUSTRIA, AUSTRIA.BIND)

# We use the alpha later for plotting
D.AUSTRIA$alpha <- 0.5
D.STATES$alpha <- 0

D.STATES <- rbind(D.AUSTRIA, D.STATES)

#### New Queens #####
D.QN2 <- D.FULL %>% select( young_queens, hives_winter, hives_lost_e, hives_spring_e, lost_rate_e, hives_spring_queen, lost_a, lost_c ) %>% na.omit()
D.QN2$young_rate <- F_NUMBER_FORMAT( D.QN2$young_queens / D.QN2$hives_winter * 100 )
# Got some supicious data, better remove them ...
D.QN2 <- D.QN2 %>% filter( young_rate <= 100 )

# Young Queen Rate BoxPlots
L.SEQ <- seq( 0, 100, 25 )
L.GROUPS <- c( "0-25%", "26-50%", "51-75%", "76-100%" )
D.QN2$young_rate_group <- cut( D.QN2$young_rate, L.SEQ, label = L.GROUPS, include.lowest = TRUE, right = TRUE )

# Create Plot DF
D.PLOT_Q <- D.QN2 %>%
  group_by(young_rate_group) %>%
  summarise(
    n = n(),
    np = F_NUMBER_FORMAT(n() / nrow(D.QN2) * 100)
  )

# We want loss rate without queen related losses
D.QN2$hives_lost_e <- D.QN2$lost_c
CACHE.BIND <- F_GLM_FACTOR( D.QN2, "young_rate_group", D.QN2$young_rate_group )

# Queen Exchange rate to overall losses
D.PLOT_Q1 <- cbind( D.PLOT_Q, CACHE.BIND )

# Queen Exchange rate to queen losses
D.QN2$hives_lost_e <- D.QN2$lost_a
D.QN2$hives_spring_e <- D.QN2$hives_spring_queen
CACHE.BIND2 <- F_GLM_FACTOR( D.QN2, "young_rate_group", D.QN2$young_rate_group )
# Queen Exchange rate to overall losses
D.PLOT_Q2 <- cbind( D.PLOT_Q, CACHE.BIND2 )

# Summary of new queens
D.SUM.Q = tibble(hives_winter = 0, young_queens = 0, young_rate = 0)
D.SUM.Q$hives_winter <- sum(D.QN2$hives_winter)
D.SUM.Q$young_queens <- sum(D.QN2$young_queens)
D.SUM.Q$young_rate <- F_NUMBER_FORMAT(D.SUM.Q$young_queens / D.SUM.Q$hives_winter * 100 )

write.csv( D.SUM.Q, file = "DSUMQ.csv" )

#### PLOTTING #####
# Ordering
OrderVector <- c( "Österreich", "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien")
RenameVector <- c( "Austria", "Burgenland", "Carinthia", "Lower Austria", "Upper Austria", "Salzburg", "Styria", "Tyrol", "Vorarlberg", "Vienna")

D.STATES$ff <- factor( D.STATES$ff, levels = OrderVector )
D.STATES <- D.STATES[ order( factor( D.STATES$ff, levels = OrderVector )),]
D.STATES$fe <- RenameVector

# Workaround because ggplot uses for alpha 0 --> 0.1 
color_rule <- ifelse(D.STATES$alpha == 0, NA, "gray")

p1 <- 
  ggplot( D.STATES, aes( x = ff, y = middle )) +
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( aes(alpha = alpha), colour = "black", fill = color_rule, show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 1.0 )+ 
  xlab("") + ylab("Queen related loss rate [%]") + 
  ggtitle("(A) Overall losses due queen problems - Austria & states") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 10, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    axis.text.y = element_text(angle = 0, size = 10),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
    labels = paste( D.STATES$fe,"\n ( n = ",D.STATES$n, " )", sep="" ),
    limits = c( levels( D.STATES$ff ))
  ) +
  scale_y_continuous(
    limit = c(0, 8.1),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 1 )
  )


p3 <- ggplot( data = D.PLOT_Q1 ) +
  aes( x = young_rate_group, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Amount of young queens [%]") + ylab("Number of beekeeper (n)") + 
  ggtitle("(B) Distribution of young queens per operation in given groups") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 1000, 100 ),
    limits = c( 0, max(D.PLOT_Q1$n)+100 )
  )

p4 <- ggplot( data = D.PLOT_Q1 ) +
  aes( x = young_rate_group, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = young_rate_group, y = 0.5, label = paste("n = ", n )), color = "black", size = 2.5 ) +
  xlab("Amount of young queens [%]") + ylab("Loss rate [%] (colonies died out during winter)") + 
  ggtitle("(C) Loss rate to relative amount of young queens w/o queen related losses") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

# Queen Exchange rate to queen related losses
p5 <- ggplot( data = D.PLOT_Q2 ) +
  aes( x = young_rate_group, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = young_rate_group, y = 0.2, label = paste("n = ", n )), color = "black", size = 2.5 ) +
  xlab("Amount of  young queens [%]") + ylab("Loss rate [%] (due to queen problems)") + 
  ggtitle("(D) Queen related loss rate to relative amount of young queens") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limits = c(0, 8.1),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 1 )
  )

#gtitle = textGrob( "Queen related losses" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2 ), c(3, 4))
p_p <- arrangeGrob( p1, p3, p4, p5,
             #top = gtitle, 
             layout_matrix = lay)

# Save File
ggsave("./img/Plot_Queen_losses.pdf", p_p, width = 12, height = 8, units = "in")

#### Plot Queen Problems #####

p2 <- 
  ggplot ( data = C.QP) +
  aes(x = ff, y = n) +
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(rate, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("") + ylab("Number of beekeepers (n)") + 
  ggtitle("(A) Subjective queen problems in comparison to last year(s)") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
    labels = paste( C.QP$ff,"\n ( n = ", C.QP$n, " )", sep="" ),
    limits = c( levels( C.QP$ff ))
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 10000, 100 ),
    limits = c( 0, max(C.QP$n)+100 )
  )

# Subjective queen problems to loss rate
p6 <- ggplot( data = C.QP ) +
  aes( x = ff, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.2, label = paste("n = ", n )), color = "black", size = 2.5 ) +
  xlab("") + ylab("Loss rate [%] (due to queen problems)") + 
  ggtitle("(C) Subjective queen problems to queen related loss rate") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limit = c(0, 8.1),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 1 )
  )

# Subjective queen problems to loss rate without queen problems
p7 <- ggplot( data = C.QP2 ) +
  aes( x = ff, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), color = "black", size = 2.5 ) +
  xlab("") + ylab("Loss rate [%] (colonies died out during winter)") + 
  ggtitle("(B) Subjective queen problems to loss rate w/o queen related losses") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limit = c(0, 21),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

#gtitle = textGrob( "Subjective queen problems" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 1 ), c( 2, 3 ) )
p_2 <- arrangeGrob( p2, p7, p6,
                    #top = gtitle, 
                    layout_matrix = lay)

# Save File
ggsave("./img/Plot_Queen_losses2.pdf", p_2, width = 11, height = 8, units = "in")



