##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### TREATMENT METHODS COMBINATIONS PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )


#### START CODE #####

# List of Factors we want in our Plot
# CAREFUL with ordering, they will be correct only if they are in order with column numbers
# commented columns are ignored to reduce number of possible combinations, as they are not worth when looked at usage histograms

oList = list(
  
  c("A", "SPRING", "Hyperthermia", "Sp-Hyp"),
  c("A", "SPRING", "Biotechnical m.", "Sp-Bio"),
  #c("A", "SPRING", "Formic acid - ST", "Sp-F-ST"),
  #c("A", "SPRING", "Formic acid - LT", "Sp-F-LT"),
  c("A", "SPRING", "Lactic acid", "Sp-Lac"),
  #c("A", "SPRING", "Ox. acid - trick. pure", "Sp-Ox-T"),
  c("A", "SPRING", "Ox. acid - sub.", "Sp-Ox-S"),
  #c("A", "SPRING", "Ox. acid - trick. mix", "Sp-Ox-M"),
  c("A", "SPRING", "Ox. acid - trick.", "Sp-Ox-T"),
  #c("A", "SPRING", "Thymol", "Sp-Thy),
  #c("A", "SPRING", "Synthetic m.", "Sp-Syn"),
  #c("A", "SPRING", "Another m.", "Sp-Oth"),
  
  c("B", "SUMMER", "Hyperthermia", "Su-Hyp"),
  c("B", "SUMMER", "Biotechnical m.", "Su-Bio"),
  c("B", "SUMMER", "Formic acid - ST", "Su-F-ST"),
  c("B", "SUMMER", "Formic acid - LT", "Su-F-LT"),
  c("B", "SUMMER", "Lactic acid", "Su-Lac"),
  #c("B", "SUMMER", "Ox. acid - trick. pure", "Su-Ox-T"),
  c("B", "SUMMER", "Ox. acid - sub.", "Su-Ox-S"),
  #c("B", "SUMMER", "Ox. acid - trick. mix.", "Su-Ox-M"),
  c("B", "SUMMER", "Ox. acid - trick.", "Su-Ox-T"),
  c("B", "SUMMER", "Thymol", "Su-Thy"),
  #c("B", "SUMMER", "Synthetic m.", "Su-Syn"),
  #c("B", "SUMMER", "Another m.", "Su-Oth"),
  
  c("C", "WINTER", "Hyperthermia", "Wi-Hyp"),
  #c("C", "WINTER", "Biotechnical m.", "Wi-Bio"),
  #c("C", "WINTER", "Formic acid - ST", "Wi-F-ST"),
  #c("C", "WINTER", "Formic acid - LT", "Wi-F-LT"),
  c("C", "WINTER", "Lactic acid", "Wi-Lac"),
  #c("C", "WINTER", "Ox. acid - trick. pure", "Wi-Ox-T"),
  c("C", "WINTER", "Ox. acid - sub.", "Wi-Ox-S"),
  #c("C", "WINTER", "Ox. acid - trick. mix.", "Wi-Ox-M")
  c("C", "WINTER", "Ox. acid - trick.", "Wi-Ox-T")
  #c("C", "WINTER", "Thymol", "Wi-Thy)
  #c("C", "WINTER", "Synthetic m.", "Wi-Syn"),
  #c("C", "WINTER", "Another m.", "Wi-Oth)
  
)

# Drop Drone brood removal columns as they would get picked up with our next regex
D.FULL <- D.FULL[, -grep("T_drone", colnames(D.FULL))]
# Remove other columns which are low represented and would only blow our combinations
D.FULL <- D.FULL[, -grep("T_formic_short_totalyn_winter", colnames(D.FULL))]
D.FULL <- D.FULL[, -grep("T_formic_long_totalyn_winter", colnames(D.FULL))]
D.FULL <- D.FULL[, -grep("T_formic_short_totalyn_spring", colnames(D.FULL))]
D.FULL <- D.FULL[, -grep("T_formic_long_totalyn_spring", colnames(D.FULL))]

D.FULL <- D.FULL[, -grep("T_biotechnical_totalyn_winter", colnames(D.FULL))]

D.FULL <- D.FULL[, -grep("T_thymol_totalyn_spring", colnames(D.FULL))]
D.FULL <- D.FULL[, -grep("T_thymol_totalyn_winter", colnames(D.FULL))]

D.FULL <- D.FULL[, -grep("T_synthetic", colnames(D.FULL))]
D.FULL <- D.FULL[, -grep("T_other", colnames(D.FULL))]

# Get column number with yn_* in it
ColComb1 <- grep("totalyn_", colnames(D.FULL), fixed = TRUE)
# Add to our oList with the names the ColNumber for better inserting later
CacheList <- data.frame(t(sapply(oList, c)))
CacheList <- cbind(CacheList, ColComb1)
# First run, means "only" treatments no combinations
CACHE.COMB <- F_COMBINATION(D.FULL, ColComb1, 1, CacheList, ColComb1)

# Numer of max. combinations, first round gets jumped because we already have done it before
# no combinations with at least 15n were found with 4 different treatments
nMaxComb <- 3

# create the max runs
for(i in 2:nMaxComb){
  # Calculate every possible combination
  ColCombN <- combn( ColComb1 , i, simplify = FALSE )
  # get our data
  CACHE.COMB.N <- F_COMBINATION(D.FULL, ColCombN, i, CacheList, ColComb1)
  # save rows to our main df
  CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB.N)
}

# We dont allow negative values, they get generated because our bootstrap CI dont know there are no negative values
CACHE.COMB$c_ci_lower[CACHE.COMB$c_ci_lower < 0] <- 0

dump <- CACHE.COMB

# Creating xAxisLetters and add it to DF
xAxisMaxLength <- nrow(CACHE.COMB)
# Small function to generate more than 26 letters eg Z AA AB 
LETTERS2<-c(LETTERS[1:26], paste0("A",LETTERS[1:26]))
xAxisTemp <- LETTERS2[1:xAxisMaxLength]
CACHE.COMB <- cbind(CACHE.COMB, xAxisTemp)
names(CACHE.COMB)[29] <- "xAxisTempOld"

# Order the Dataframe by n
ordered_DF <- CACHE.COMB[order(CACHE.COMB$n, decreasing = TRUE),]
CACHE.COMB <- ordered_DF
CACHE.COMB <- cbind(CACHE.COMB, xAxisTemp)

# Save to File, before we need to convert it into a matrix. Dont know why, seems some problems with the data format.
CACHE.COMB.MATRIX <- as.matrix(CACHE.COMB)
write.csv( CACHE.COMB.MATRIX, file = paste("./", "Combination_Treatments.csv", sep = "" ) )

# Our Plot subset
CACHE.COMB.PLOT <- CACHE.COMB[CACHE.COMB$n > 14, ]

# We remove H because the wide range
#CACHE.COMB.PLOT <- CACHE.COMB[CACHE.COMB$xAxisTemp != "H", ]

CACHE.COMB.MATRIX <- as.matrix(CACHE.COMB.PLOT)
write.csv( CACHE.COMB.MATRIX, file = paste("./", "Combination_Treatments_Plot.csv", sep = "" ) )

# list of "good" visible forms
shapeForms <- c(1:18,33:127)
shapeLetters <- c(65:79, 80:90)

# remove "H" because we removed it from the PLOT
#shapeLetters <- shapeLetters[shapeLetters != 72]

# list of "distinctiv" colors, actually not used
disColor20 <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabebe", "#469990","#e6beff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#a9a9a9", "#172ab7")
shadesOfGrey <- colorRampPalette(c("grey50", "grey100"))

# rename our letter column so we dont need to fiddle with the plot code
names(CACHE.COMB.PLOT)[names(CACHE.COMB.PLOT) == 'xAxisTemp'] <- 'Combination'
CACHE.COMB.PLOT$'Combination (ny/nx)' <- ""
CACHE.COMB.PLOT$'Combination (ny/nx)' <- paste("(", CACHE.COMB.PLOT$Combination, ")", CACHE.COMB.PLOT$short, " (", CACHE.COMB.PLOT$n, " / ", CACHE.COMB.PLOT$c_n, " )", sep = "")

# create limits for point plot with custom column for arrow heads
CACHE.COMB.PLOT.POINT <- CACHE.COMB.PLOT

CACHE.COMB.PLOT.POINT <- CACHE.COMB.PLOT.POINT %>% mutate(upperlim_max = ifelse(upperlim > 40, 40-middle, NA))
CACHE.COMB.PLOT.POINT <- CACHE.COMB.PLOT.POINT %>% mutate(c_ci_upper_max = ifelse(c_ci_upper > 30, 30-c_mean, NA))

CACHE.COMB.PLOT.POINT <- CACHE.COMB.PLOT.POINT %>% mutate(upperlim = ifelse(upperlim > 40, middle, upperlim))
CACHE.COMB.PLOT.POINT <- CACHE.COMB.PLOT.POINT %>% mutate(c_ci_upper = ifelse(c_ci_upper > 30, c_mean, c_ci_upper))

p1 <- 
  ggplot( CACHE.COMB.PLOT.POINT, aes( x = c_mean, y = middle, shape = `Combination (ny/nx)`) ) +
  # errorbar vertical
  geom_errorbar( aes( ymin = lowerlim, ymax = upperlim, width = 0.5 ), color = "gray", linetype = "solid", size = 1, alpha = 0.7, show.legend = FALSE, na.rm = TRUE ) + 
  # errorbar horizontal
  geom_errorbarh( aes( xmin = c_ci_lower, xmax = c_ci_upper, height = 0.5), color = "gray", linetype = "solid", alpha = 0.7, size = 1, show.legend = FALSE, na.rm = TRUE ) +
 
   # Arrow Heads if bars are longer than plot
  geom_segment(aes(x = c_mean, xend = c_mean, y = middle, yend = middle + upperlim_max), 
               arrow = arrow(length = unit(0.03, "npc")),
               na.rm = TRUE, arrow.fill = "black", color = "gray", linetype = "solid", alpha = 0.7, size = 1) + 
  geom_segment(aes(x = c_mean, xend = c_mean + c_ci_upper_max, y = middle, yend = middle), 
               arrow = arrow(length = unit(0.03, "npc")),
               na.rm = TRUE, arrow.fill = "black", color = "gray", linetype = "solid", alpha = 0.7, size = 1) + 
  
  # background point
  geom_point( shape = 21, size = 10, fill = "black", color = "black", show.legend = TRUE) + 
  # point with symbol
  geom_point( size = 5, color = "white" ) + 
  # use defined shapes and color with better visibility
  scale_shape_manual( values = shapeLetters[1:20] ) +


  # custom text repel that text dont overlap
  #geom_label_repel( show.legend = FALSE, hjust = "right", nudge_x = -0.3, nudge_y = -0.9, fontface = "bold", color = "black", segment.alpha = 0) +

  xlab("Cost of treatment materials per colony [Euro]") + ylab("Loss rate [%]") + labs( colour = "Combination ( y(n) / x(n) )", shape = "Combination ( y(n) / x(n) )", fill = "Combination ( y(n) / x(n) )" ) + 
  ggtitle("Combination of treatment methods - loss rate to cost per colony") +
  
  coord_cartesian(ylim=c(0, 40), xlim=c(0, 30)) + 
  
  
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20), 
    axis.title = element_text(colour = "black", size = 14), 
    axis.text = element_text(size = 11 ), 
    
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
    #limits = c(0, 40)
  )

p2 <- 
  ggplot( CACHE.COMB.PLOT, aes( x = Combination, y = middle, shape = `Combination (ny/nx)` )) +
  geom_bar( alpha = 0, fill = "white", show.legend = FALSE, color = "gray20", stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), color = "gray20", size = 1.0, show.legend = FALSE ) + 
  # background point
  geom_point( shape = 21, size = 7, fill = "black", color = "black", show.legend = FALSE) + 
  # point with symbol
  geom_point( size = 3, stroke = 1, show.legend = FALSE, color = "white") + 
  # use defined shapes and color with better visibility
  scale_shape_manual( values = shapeLetters[1:20] ) +

  geom_text( aes( x = Combination, y = 2, label = paste("n = ", n )), angle = 0, color = "black", size = 3 ) +
  ggtitle("(A) Overall loss rate") +
  xlab("") + ylab("Loss rate [%]") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

p3 <- 
  ggplot( CACHE.COMB.PLOT, aes( x = Combination, y = c_mean, shape = `Combination (ny/nx)` )) +
  geom_bar( alpha = 0, fill = "white", color = "gray20", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = c_ci_lower, ymax = c_ci_upper ), color = "gray20", size = 1.0, show.legend = FALSE ) + 
  geom_text( aes( x = Combination, y = 2, label = paste("n = ", c_n )), angle = 0, color = "black", size = 3 ) +
  # background point
  geom_point( shape = 21, size = 7, fill = "black", color = "black", show.legend = FALSE) + 
  # point with symbol
  geom_point( size = 3, stroke = 1, color = "white", show.legend = FALSE) + 
  # use defined shapes and color with better visibility
  scale_shape_manual( values = shapeLetters[1:20] ) +
  
  xlab("") + ylab("Cost per colony [Euro]") + 
  ggtitle("(B) Cost of treatment materials per colony") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

gtitle = textGrob( "Combination of treatment methods" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ), c( 2 ) )

p <- arrangeGrob( p2, p3,
                  top = gtitle, 
                  layout_matrix = lay)

ggsave("./img/Plot_Treatment_Combination1.pdf", p, width = 14, height = 8, units = "in")

ggsave("./img/Plot_Treatment_Combination2.pdf", p1, width = 14, height = 8.5, units = "in")


threecolors <- c("white", "gray", "black")
threetextcolors <- c("black", "black", "white")
CACHE.COMB.PLOT$circle_color <- threecolors[CACHE.COMB.PLOT$t]
CACHE.COMB.PLOT$text_color <- threetextcolors[CACHE.COMB.PLOT$t]
CACHE.COMB.PLOT$t <- as.factor(CACHE.COMB.PLOT$t)

p4 <- 
  ggplot( CACHE.COMB.PLOT, aes( x = Combination, y = middle, shape = `Combination (ny/nx)` )) +
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( alpha = 0, fill = "white", show.legend = FALSE, color = "gray20", stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), color = "gray20", size = 1.0, show.legend = FALSE ) + 
  # background point
  geom_point( aes(fill = t), shape = 21, size = 7, color = "black", show.legend = FALSE) + 
  geom_point( aes(colour = t), size = 3, stroke = 1, show.legend = FALSE) + 
  # use defined shapes and color with better visibility
  scale_shape_manual( values = shapeLetters[1:20] ) +
  scale_fill_manual(values=threecolors) +
  scale_colour_manual(values = threetextcolors) +
  
  geom_text( aes( x = Combination, y = 0.5, label = paste("n = ", n )), angle = 0, color = "black", size = 3 ) +
  #ggtitle("Combination of treatment methods loss rates") +
  xlab("") + ylab("Loss rate [%]") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=20), 
    axis.title.x = element_text(colour = "black", size = 15 ), 
    axis.title.y = element_text(colour = "black", size = 15 ), 
    
    axis.text.x = element_text(angle = 0, size = 13, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 13, face = "bold"),
    
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

ggsave("./img/Plot_Treatment_Combination3.pdf", p4, width = 12, height = 6.5, units = "in")
