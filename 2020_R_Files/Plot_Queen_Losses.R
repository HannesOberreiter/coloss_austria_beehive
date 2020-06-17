##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
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

#### Start Code ####
D.FULL <- D.RAW

#### Queen Problems #####
V.label <- c("Häufiger", "Normal", "Seltener", "Weiß nicht")

D.QP      <- F_EXTRACT_N( D.FULL, "queen_problems", "Queen Problems" )
D.QP$rate <- F_NUMBER_FORMAT( D.QP$n / sum(D.QP$n) * 100)

D.CACHE <- D.FULL
# queen loss rate - queen problems
D.CACHE$hives_lost_e <- D.CACHE$lost_a
D.CACHE$hives_spring_e <- D.CACHE$hives_spring_queen
D.QP.GLM <- F_GLM_FACTOR( D.CACHE, "queen_problems", D.CACHE$queen_problems, TRUE)
D.QP.GLM <- cbind(D.QP, D.QP.GLM)
D.QP.GLM$c <- "Queen Loss Rate"
# colony loss rate w/o queen loss rate -- queen problems
D.CACHE <- D.FULL
D.CACHE$hives_lost_e <- D.CACHE$lost_c
D.CACHE$hives_spring_e <- ifelse( is.na( D.CACHE$lost_c ), NA, D.CACHE$hives_winter - D.CACHE$lost_c )
D.QP.GLM2 <- F_GLM_FACTOR( D.CACHE, "queen_problems", D.CACHE$queen_problems, TRUE )
D.QP.GLM2 <- cbind(D.QP, D.QP.GLM2)
D.QP.GLM2$c <- "Colony Loss Rate"
# combine the two different loss rate caluculations
D.QP <- rbind(D.QP.GLM, D.QP.GLM2)
# cleanup
rm(D.QP.GLM, D.QP.GLM2, D.CACHE)
# Ordering
D.QP$ff <- factor( D.QP$ff, levels = V.label )
D.QP    <- D.QP[order(factor(D.QP$ff, levels = V.label )),]
# Plotting
PLOT.QP1 <- F_SINGLE_PLOT(D.QP[D.QP$c == "Queen Loss Rate",], ptitle = "(A) Verlustrate nur mit Verluste durch Königinnen Probleme")
D.ANNOTATION <- F_CHISTAR_DF(D.QP[D.QP$c == "Queen Loss Rate",], "Häufiger", "Normal", "c")
D.ANNOTATION <- rbind(D.ANNOTATION, F_CHISTAR_DF(D.QP[D.QP$c == "Queen Loss Rate",], "Häufiger", "Seltener", "c"))
D.ANNOTATION <- rbind(D.ANNOTATION, F_CHISTAR_DF(D.QP[D.QP$c == "Queen Loss Rate",], "Häufiger", "Weiß nicht", "c"))
D.ANNOTATION$y <- D.ANNOTATION$y * c(1, 1.1, 1.2)
if(nrow(D.ANNOTATION)> 0){
  PLOT.QP1 <- PLOT.QP1 + geom_signif(data=D.ANNOTATION, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE, vjust=0.5)
}

PLOT.QP2 <- F_SINGLE_PLOT(D.QP[D.QP$c == "Colony Loss Rate",], ptitle = "(B) Verlustrate ohne Verluste durch Königinnen Probleme")
D.ANNOTATION <- F_CHISTAR_DF(D.QP[D.QP$c == "Colony Loss Rate",], "Häufiger", "Normal", "c")
D.ANNOTATION <- rbind(D.ANNOTATION, F_CHISTAR_DF(D.QP[D.QP$c == "Colony Loss Rate",], "Häufiger", "Seltener", "c"))
D.ANNOTATION$y <- D.ANNOTATION$y * c(1, 1.1)
if(nrow(D.ANNOTATION)> 0){
  PLOT.QP2 <- PLOT.QP2 + geom_signif(data=D.ANNOTATION, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE, vjust = 0.5)
}

lay <- rbind( c( 1 ), c( 2 ) )
p1 <- arrangeGrob( PLOT.QP1, PLOT.QP2,layout_matrix = lay)
ggsave("./img/plot_queen_subjectiveproblems.pdf", p1, width = 11, height = 8, units = "in")
rm(lay, V.label)
D.QP$latex <- F_LATEX_CONF(D.QP)

#### TOTAL Lost Queen Rate #####
# replace our columns with correct values for queen loss as we use our function
D.QUEEN.FULL <- D.FULL
D.QUEEN.FULL$hives_lost_e <- D.QUEEN.FULL$lost_a
D.QUEEN.FULL$hives_spring_e <- D.QUEEN.FULL$hives_spring_queen
# States
D.STATES   <- F_EXTRACT_N(D.QUEEN.FULL, "state", "STATES")
CACHE.BIND <- F_GLM_FACTOR( D.QUEEN.FULL, "state", D.QUEEN.FULL$state )
D.STATES   <- cbind(D.STATES, CACHE.BIND)
# Austria
D.FULL.AUSTRIA <- D.QUEEN.FULL
D.FULL.AUSTRIA$state <- "Österreich"
D.AUSTRIA <- F_EXTRACT_N(D.FULL.AUSTRIA, "state", "STATES")
AUSTRIA.BIND <- F_GLM_SINGLE( D.QUEEN.FULL )
D.AUSTRIA <- cbind(D.AUSTRIA, AUSTRIA.BIND)
# We use the alpha later for plotting
D.AUSTRIA$alpha <- 'grey'
D.STATES$alpha  <- 'white'
D.STATES <- rbind(D.AUSTRIA, D.STATES)
# Ordering
V.label <- c( "Österreich", "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien")
D.STATES <- D.STATES[ order( factor( D.STATES$ff, levels = V.label )),]
D.STATES$ff <- c("AUT", "Bgld.", "Ktn.", "NÖ", "OÖ", "Sbg.", "Stmk.", "T", "Vbg.", "W")
p2 <- F_SINGLE_PLOT(D.STATES, barfill = D.STATES$alpha)
ggsave("./img/plot_queen_states.pdf", p2, width = 6, height = 3.5, units = "in")
D.STATES$latex <- F_LATEX_CONF(D.STATES)
rm(AUSTRIA.BIND, CACHE.BIND, D.AUSTRIA, D.FULL.AUSTRIA, D.QUEEN.FULL, V.label)

#### New Queens #####
# Splitting Sequence
V.SEQ <- seq( 0, 100, 25 )
V.GROUPS <- c( "0-25%", "26-50%", "51-75%", "76-100%" )

D.YOUNG.QUEENS <- D.FULL %>% select( 
  young_queens, hives_winter, 
  hives_lost_e, hives_spring_e, lost_rate_e, lost_b,
  hives_spring_queen, lost_a, lost_c ) %>% na.omit()
D.YOUNG.QUEENS$young_rate <- F_NUMBER_FORMAT(D.YOUNG.QUEENS$young_queens / D.YOUNG.QUEENS$hives_winter * 100 )

# Participants with "wrong" answers, we max at 100%, slack message Robert Brodschneider 25.05.2020
D.YOUNG.QUEENS$young_rate[D.YOUNG.QUEENS$young_rate > 100] <- 100
# D.YOUNG.QUEENS <- D.YOUNG.QUEENS %>% filter( young_rate <= 100 )

D.YOUNG.QUEENS$young_rate_group <- cut(
  D.YOUNG.QUEENS$young_rate,
  V.SEQ, label = V.GROUPS,
  include.lowest = TRUE, right = TRUE )

# Create Plot DF
D.YOUNG.GROUP   <- F_EXTRACT_N(D.YOUNG.QUEENS, "young_rate_group", "young_rate_group")

# Queen Exchange rate to queen losses
D.YOUNG.QUEENS$hives_lost_e <- D.YOUNG.QUEENS$lost_a
D.YOUNG.QUEENS$hives_spring_e <- D.YOUNG.QUEENS$hives_spring_queen
D.GLM1 <- F_GLM_FACTOR( D.YOUNG.QUEENS, "young_rate_group", D.YOUNG.QUEENS$young_rate_group, TRUE)
D.GLM1 <- cbind( D.YOUNG.GROUP, D.GLM1 )
D.GLM1$c <- "Queen Loss Rate"

# Loss rate without queen loss rate
D.YOUNG.QUEENS$hives_lost_e <- D.YOUNG.QUEENS$lost_c
D.YOUNG.QUEENS$hives_spring_e <- ifelse( is.na( D.YOUNG.QUEENS$lost_c ), NA, D.YOUNG.QUEENS$hives_winter - D.YOUNG.QUEENS$lost_c )
D.GLM2 <- F_GLM_FACTOR( D.YOUNG.QUEENS, "young_rate_group", D.YOUNG.QUEENS$young_rate_group, TRUE)
D.GLM2 <- cbind(D.YOUNG.GROUP, D.GLM2)
D.GLM2$c <- "Colony Loss Rate"

# combine the two different loss rate together into one DF
D.YOUNG.GROUP <- rbind(D.GLM1, D.GLM2)

rm(D.GLM1, D.GLM2)

# Summary of new queens
D.SUM.Q = tibble(
  hives_winter = sum(D.YOUNG.QUEENS$hives_winter), 
  young_queens = sum(D.YOUNG.QUEENS$young_queens), 
  young_rate = 0)
D.SUM.Q$young_rate <- F_NUMBER_FORMAT(D.SUM.Q$young_queens / D.SUM.Q$hives_winter * 100 )

write.csv( D.SUM.Q, file = "Young_Queens_Total.csv" )

# Ordering
D.YOUNG.GROUP$ff <- factor( D.YOUNG.GROUP$ff, levels = V.GROUPS )
D.YOUNG.GROUP    <- D.YOUNG.GROUP[order(factor(D.YOUNG.GROUP$ff, levels = V.GROUPS )),]
# Plotting
PLOT.QP1 <- F_SINGLE_PLOT(D.YOUNG.GROUP[D.YOUNG.GROUP$c == "Queen Loss Rate",], ptitle = "(A) Verlustrate nur mit Verluste durch Königinnen Probleme")
PLOT.QP2 <- F_SINGLE_PLOT(D.YOUNG.GROUP[D.YOUNG.GROUP$c == "Colony Loss Rate",], ptitle = "(B) Verlustrate ohne Verluste durch Königinnen Probleme")

D.ANNOTATION <- F_CHISTAR_DF(D.YOUNG.GROUP[D.YOUNG.GROUP$c == "Colony Loss Rate",], "0-25%", "26-50%", "c")
D.ANNOTATION <- rbind(D.ANNOTATION, F_CHISTAR_DF(D.YOUNG.GROUP[D.YOUNG.GROUP$c == "Colony Loss Rate",], "0-25%", "51-75%", "c"))
D.ANNOTATION <- rbind(D.ANNOTATION, F_CHISTAR_DF(D.YOUNG.GROUP[D.YOUNG.GROUP$c == "Colony Loss Rate",], "0-25%", "76-100%", "c"))
D.ANNOTATION$y <- D.ANNOTATION$y * c(0.97, 1.02, 1.07)
D.ANNOTATION <- rbind(D.ANNOTATION, F_CHISTAR_DF(D.YOUNG.GROUP[D.YOUNG.GROUP$c == "Colony Loss Rate",], "26-50%", "51-75%", "c"))
if(nrow(D.ANNOTATION)> 0){
  PLOT.QP2 <- PLOT.QP2 + geom_signif(data=D.ANNOTATION, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE, vjust = 0.5)
}
lay <- rbind( c( 1 ), c( 2 ) )
p1 <- arrangeGrob( PLOT.QP1, PLOT.QP2,layout_matrix = lay)
ggsave("./img/plot_queen_exchangerate.pdf", p1, width = 11, height = 8, units = "in")
D.YOUNG.GROUP$latex <- F_LATEX_CONF(D.YOUNG.GROUP)
