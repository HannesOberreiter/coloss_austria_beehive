##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### TREATMENT METHODS COMBINATIONS PLOT ###########

# Set Working directory (uses API of RStudio)
#SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
#setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

# List of Factors we want in our Plot
# CAREFUL with ordering, they will be correct only if they are in order with column numbers
# commented columns are ignored to reduce number of possible combinations, as they are not worth when looked at usage histograms

V.oList = list(
  
  c("A", "FRÜHJAHR", "Hyperthermie",                 "F-Hyp. "),
  c("A", "FRÜHJAHR", "Andere biotechnische Methode", "F-Biot. "),
  c("A", "FRÜHJAHR", "Ameisensäure - Kurzzeit",      "F-AS-KZ "),
  c("A", "FRÜHJAHR", "Ameisensäure - Langzeit",      "F-AS-LZ "),
  c("A", "FRÜHJAHR", "Milchsäure",                   "F-Milchs. "),
  c("A", "FRÜHJAHR", "Oxalsäure - sub.",             "F-Ox-Sub. "),
  c("A", "FRÜHJAHR", "Oxalsäure - träufeln",         "F-Ox-Träu. "),
  c("A", "FRÜHJAHR", "Thymol",                       "F-Thy "),
  c("A", "FRÜHJAHR", "Anderes chem. Produkt",        "F-chem. Pr. "),
  c("A", "FRÜHJAHR", "Andere Methode",               "F-Andere "),
  
  c("B", "SOMMER", "Hyperthermie",                 "S-Hyp."),
  c("B", "SOMMER", "Andere biotechnische Methode", "S-Biot. "),
  c("B", "SOMMER", "Ameisensäure - Kurzzeit",      "S-AS-KZ "),
  c("B", "SOMMER", "Ameisensäure - Langzeit",      "S-AS-LZ "),
  c("B", "SOMMER", "Milchsäure",                   "S-Milchs. "),
  c("B", "SOMMER", "Oxalsäure - sub.",             "S-Ox-Sub. "),
  c("B", "SOMMER", "Oxalsäure - träufeln",         "S-Ox-Träu. "),
  c("B", "SOMMER", "Thymol",                       "S-Thy. "),
  c("B", "SOMMER", "Anderes chem. Produkt",        "S-chem. Pr. "),
  c("B", "SOMMER", "Andere Methode",               "S-Andere "),
  
  c("C", "WINTER", "Hyperthermie",                 "W-Hyp. "),
  c("C", "WINTER", "Andere biotechnische Methode", "W-Biot. "),
  c("C", "WINTER", "Ameisensäure - Kurzzeit",      "W-AS-KZ "),
  c("C", "WINTER", "Ameisensäure - Langzeit",      "W-AS-LZ "),
  c("C", "WINTER", "Milchsäure",                   "W-Milchs. "),
  c("C", "WINTER", "Oxalsäure - sub.",             "W-Ox-Sub. "),
  c("C", "WINTER", "Oxalsäure - träufeln",         "W-Ox-Träu. "),
  c("C", "WINTER", "Thymol",                       "W-Thy. "),
  c("C", "WINTER", "Anderes chem. Produkt",        "W-chem. Pr. "),
  c("C", "WINTER", "Andere Methode",               "W-Andere ")
  
)

# Drop Drone Brood and Varroa Count columns as they would get picked up with our next regex
D.FULL <- D.FULL[, -grep("T_drone", colnames(D.FULL))]
D.FULL <- D.FULL[, -grep("T_vcount", colnames(D.FULL))]

# TODO don't remove columns otherwise participants with some combination will not be included into other combination even if it is not true
# Remove other columns which are low represented and would only blow our combinations
#D.FULL <- D.FULL[, -grep("T_formic_short_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_formic_long_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_formic_short_totalyn_spring", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_formic_long_totalyn_spring", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_biotechnical_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_thymol_totalyn_spring", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_thymol_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_synthetic", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_other", colnames(D.FULL))]

# Get column number with yn_* in it
V.ColComb1  <- grep("totalyn_", colnames(D.FULL), fixed = TRUE)
# Add to our oList with the names the ColNumber for better inserting later
V.CacheList <- data.frame(t(sapply(V.oList, c)))
rm(V.oList)
V.CacheList <- cbind(V.CacheList, V.ColComb1)

#### Calculate Combinations ####

# First run, means "only" treatments no combinations
CACHE.COMB <- F_COMBINATION(D.FULL, V.ColComb1, 1, V.CacheList, V.ColComb1)

# Numer of max. combinations, first round gets jumped because we already have done it before
# no combinations with at least 15n were found with 4 different treatments
V.nMaxComb <- 3

# create the max runs
for(i in 2:V.nMaxComb){
  # Calculate every possible combination
  V.ColCombN <- combn( V.ColComb1 , i, simplify = FALSE )
  # get our data
  CACHE.COMB.N <- F_COMBINATION(D.FULL, V.ColCombN, i, V.CacheList, V.ColComb1)
  # save rows to our main df
  CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB.N)
}
rm(i, V.nMaxComb, CACHE.COMB.N, V.ColCombN, V.CacheList)
# We dont allow negative values, they get generated because our bootstrap CI dont know there are no negative values
CACHE.COMB$c_ci_lower[CACHE.COMB$c_ci_lower < 0] <- 0
# Safety Dump
#dump <- CACHE.COMB
#CACHE.COMB <- dump

#### Creating Letters ####

# Creating xAxisLetters and add it to DF
# Small function to generate more than 26 letters eg Z AA AB 
V.LETTERS2  <- c(LETTERS[1:26], paste0("A",LETTERS[1:26]))
V.xAxisTemp <- V.LETTERS2[1:nrow(CACHE.COMB)]
CACHE.COMB  <- cbind(CACHE.COMB, letterBeforeOrdering = V.xAxisTemp)

# Order the Dataframe by n
CACHE.COMB <- CACHE.COMB[order(CACHE.COMB$n, decreasing = TRUE),]
CACHE.COMB <- cbind(CACHE.COMB, letters = V.xAxisTemp)

# Save to File, before we need to convert it into a matrix. Dont know why, seems some problems with the data format.
CACHE.COMB.MATRIX <- as.matrix(CACHE.COMB)
write.csv( CACHE.COMB.MATRIX, file = paste("./", "Combination_Treatments.csv", sep = "" ) )
rm(CACHE.COMB.MATRIX)
rm(V.LETTERS2, V.xAxisTemp)

##### Subset our Combination Dataframe ####
D.COMB <- CACHE.COMB[CACHE.COMB$n > 14, ]

# Export Subset
CACHE.COMB.MATRIX <- as.matrix(D.COMB)
write.csv( CACHE.COMB.MATRIX, file = paste("./", "Combination_Treatments_Subset.csv", sep = "" ) )
rm(CACHE.COMB.MATRIX)

#### PLOT SHAPES, COLORS, Col Names ####
# list of "good" visible forms
V.shapeForms   <- c(1:18,33:127)
V.shapeLetters <- c(65:79, 80:90)
# list of "distinctiv" colors, actually not used
V.disColor20   <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabebe", "#469990","#e6beff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#a9a9a9", "#172ab7")
V.shadesOfGrey <- colorRampPalette(c("grey50", "grey100"))

# rename our letter column so we dont need to fiddle with the plot code
names(D.COMB)[names(D.COMB) == 'letters'] <- 'Kombination'
D.COMB$'Kombination (ny/nx)' <- ""
D.COMB$'Kombination (ny/nx)' <- paste("(", D.COMB$Kombination, ") ", D.COMB$short, " (", D.COMB$n, " / ", D.COMB$c_n, " )", sep = "")
D.COMB$'Kombination (ny/nx)' <- str_replace(D.COMB$'Kombination (ny/nx)', "&", "& ")

#### Point Plot ####
# create limits for point plot with custom column for arrow heads
D.PLOT.POINT <- D.COMB
D.PLOT.POINT <- D.PLOT.POINT %>% mutate(upperlim_max = ifelse(upperlim > 40, 40-middle, NA))
D.PLOT.POINT <- D.PLOT.POINT %>% mutate(c_ci_upper_max = ifelse(c_ci_upper > 30, 30-c_mean, NA))
D.PLOT.POINT <- D.PLOT.POINT %>% mutate(upperlim = ifelse(upperlim > 40, middle, upperlim))
D.PLOT.POINT <- D.PLOT.POINT %>% mutate(c_ci_upper = ifelse(c_ci_upper > 30, c_mean, c_ci_upper))

PLOT.POINT <- 
  ggplot( D.PLOT.POINT, 
          aes( x = c_mean, y = middle, shape = `Kombination (ny/nx)`) ) +
  geom_errorbar( 
    aes( ymin = lowerlim, ymax = upperlim, width = 0.5 ), 
    color = "gray", linetype = "solid", size = 1, alpha = 0.7, show.legend = FALSE, na.rm = TRUE ) + 
  geom_errorbarh( 
    aes( xmin = c_ci_lower, xmax = c_ci_upper, height = 0.5), 
    color = "gray", linetype = "solid", alpha = 0.7, size = 1, show.legend = FALSE, na.rm = TRUE ) +
   # Arrow Heads if bars are longer than plot
  geom_segment(
    aes(x = c_mean, xend = c_mean, y = middle, yend = middle + upperlim_max), 
               arrow = arrow(length = unit(0.03, "npc")),
               na.rm = TRUE, arrow.fill = "black", color = "gray", linetype = "solid", alpha = 0.7, size = 1) + 
  geom_segment(
    aes(x = c_mean, xend = c_mean + c_ci_upper_max, y = middle, yend = middle), 
               arrow = arrow(length = unit(0.03, "npc")),
               na.rm = TRUE, arrow.fill = "black", color = "gray", linetype = "solid", alpha = 0.7, size = 1) + 
  # background point
  geom_point( shape = 21, size = 10, fill = "black", color = "black", show.legend = TRUE) + 
  # point with symbol
  geom_point( size = 5, color = "white" ) + 
  # use defined shapes and color with better visibility
  scale_shape_manual( values = V.shapeLetters[1:20] ) +
  # custom text repel that text dont overlap
  #geom_label_repel( show.legend = FALSE, hjust = "right", nudge_x = -0.3, nudge_y = -0.9, fontface = "bold", color = "black", segment.alpha = 0) +
  xlab("Cost of treatment materials per colony [Euro]") + ylab("Loss rate [%]") + 
  labs( colour = "Combination ( y(n) / x(n) )", shape = "Combination ( y(n) / x(n) )", fill = "Combination ( y(n) / x(n) )" ) + 
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

ggsave("./img/plot_combination_point.pdf", PLOT.POINT, width = 14, height = 8, units = "in")

#### PRICE PLOT ####

PLOT.COST <- 
  ggplot( 
    D.COMB, 
    aes( x = Kombination, y = c_mean, shape = `Kombination (ny/nx)` )) +
  geom_bar( 
    alpha = 0, fill = "white", color = "gray20", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( 
    aes( ymin = c_ci_lower, ymax = c_ci_upper ), 
    color = "gray20", size = 1.0, show.legend = FALSE ) + 
  geom_text( 
    aes( x = Kombination, y = 2, label = paste("n = ", c_n )), 
    angle = 0, color = "black", size = 3 ) +
  geom_point( shape = 21, size = 7, fill = "black", color = "black", show.legend = FALSE) + 
  geom_point( size = 3, stroke = 1, color = "white", show.legend = FALSE) + 
  scale_shape_manual( values = V.shapeLetters[1:20] ) +
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
    breaks = seq( 0, max(D.COMB$c_ci_upper)*1.1, 100 ),
    limits = c( 0, max(D.COMB$c_ci_upper)*1.1 )
  )

ggsave("./img/plot_combination_cost.pdf", PLOT.COST, width = 14, height = 8, units = "in")

#### VERTICAL LOSS PLOT ####
V.threecolors       <- c("white", "gray", "black")
V.threetextcolors   <- c("black", "black", "white")
D.COMB$circle_color <- V.threecolors[D.COMB$t]
D.COMB$text_color   <- V.threetextcolors[D.COMB$t]
D.COMB$t            <- as.factor(D.COMB$t)
D.COMB$short        <- str_replace_all(D.COMB$short, "&", "& ")
# Austria Total Loss Rate
AUSTRIA.BIND <- F_GLM_SINGLE( D.RAW )

D.COMB <- D.COMB[order(D.COMB$n, decreasing = TRUE),]
D.COMB$short <- factor(D.COMB$short, levels = D.COMB$short)

D.COMB$latex <- F_LATEX_CONF(D.COMB)

PLOT.LOSS <- 
  ggplot(
    D.COMB, 
    aes( y = short, x = middle, shape = `Kombination (ny/nx)` )) +
  geom_crossbar(
    aes( xmin = lowerlim, xmax = upperlim ), fill = "white") +
  geom_vline(xintercept = AUSTRIA.BIND[1], linetype="dashed", color = "red", size=0.5) +
  geom_vline(xintercept = AUSTRIA.BIND[2], color = "red", size=0.5) + 
  geom_vline(xintercept = AUSTRIA.BIND[3], linetype="dashed", color = "red", size=0.5) +
  geom_point(
    aes(fill = t), 
    shape = 21, size = 7, color = "black", show.legend = FALSE) + 
  geom_point(
    aes(colour = t), 
    size = 3, stroke = 1, show.legend = FALSE) + 
  scale_shape_manual( values = V.shapeLetters[1:20] ) +
  scale_fill_manual(  values = V.threecolors) +
  scale_colour_manual(values = V.threetextcolors) +
  geom_text(
    aes( y = short, x = 2, label = paste("n = ", n )), 
    angle = 0, color = "black", size = 3 ) +
  #ggtitle("Combination of treatment methods loss rates") +
  ylab("") + xlab("Verlustrate [%]") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=20), 
    axis.title.x = element_text(colour = "black", size = 15 ), 
    axis.title.y = element_text(colour = "black", size = 15 ), 
    
    axis.text.x = element_text(angle = 0, size = 13, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 13, face = "bold"),
    
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.x = element_line( colour = "grey" ),
    #panel.grid.minor.x = element_line( colour = "grey" )
  ) +
  scale_y_discrete(
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

write_excel_csv2(D.COMB, "comb.csv")

ggsave("./img/plot_combination_loss.pdf", PLOT.LOSS, width = 12, height = 6.5, units = "in")
