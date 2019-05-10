##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OPTERATION FACTOR PLOT ###########

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

#### Treatment Basic Factors ####
# List of Factors we want in our Plot
oList = list(
  c("varroa_checked", "(A) Checked for varroa mite count"),
  c("varroa_treated", "(B) Did treatmend agains varroa mites"),
  c("crippled_bees", "(C) Amount of crippled bees observed")
)

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
    )

# Loop through list and create for all factors CI
for( i in oList){
  temp.n <- get( i[1], pos = D.FULL )
  # remove "Unsicher" because AINOVA Test?
  # D.FULL_C <- subset( D.FULL, temp.n != "Unsicher" )
  D.FULL_C <- D.FULL
  CACHE.M <- F_EXTRACT_N( D.FULL_C, i[1], i[2] )
  CACHE.BIND <- F_GLM_FACTOR( D.FULL_C, i[1], get( i[1], pos = D.FULL_C ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Translate Factors into english for plotting
D.FACTORS.PLOT <- D.FACTORS
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Ja" ] <- "Yes"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Unsicher" ] <- "Uncertain"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Nein" ] <- "No"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Häufig" ] <- "Often"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Überhaupt nicht" ] <- "None"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Wenig" ] <- "Seldom"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Weiß nicht" ] <- "Don't know"

# Ordering
D.FACTORS.PLOT$ff <- factor( D.FACTORS.PLOT$ff, 
                             levels = c( "Yes", "No", "Uncertain", "Often", "Seldom", "None", "Don't know"))

#### Treatment Types Overview #####
# List of Factors we want in our Plot
oList = list(
  c("T_drone_", "T_drone_total", "Dronecomb Removal"),
  c("T_hyperthermia_", "T_hyperthermia_total", "Hyperthermia"),
  c("T_biotechnical_", "T_biotechnical_total", "Other Biotechnical Method"),
  c("T_formic_short_", "T_formic_short_total", "Formic Acid - Short Term"),
  c("T_formic_long_", "T_formic_long_total", "Formic Acid - Long Term"),
  c("T_lactic_", "T_lactic_total", "Lactic Acid"),
  c("T_oxalic_trickle_", "T_oxalic_trickle_total", "Oxalic Acid - Trickle"),
  c("T_oxalic_vapo_", "T_oxalic_vapo_total", "Oxalic Acid - Vaporize"),
  c("T_oxalic_mix_", "T_oxalic_mix_total", "Oxalic Acid - Mixture"),
  c("T_thymol_", "T_thymol_total", "Thymol"),
  #c("T_synthetic_apistan_", "T_apistan_total", "Tau-fluvalinat"),
  #c("T_synthetic_flumethrin_", "T_flumethrin_total", "Flumethrin"),
  #c("T_synthetic_amitraz_strips_", "T_amitraz_strips_total", "Amitraz - Strips"),
  #c("T_synthetic_amitraz_vapo_", "T_amitraz_vapo_total", "Amitraz - Vaporize"),
  #c("T_synthetic_coumaphos_p_", "T_coumaphos_p_total", "Coumaphos - Perizin"),
  #c("T_synthetic_coumaphos_c_", "T_coumaphos_c_total", "Coumaphos - Checkmite+"),
  #c("T_synthetic_synother_", "T_chemical_total", "Other Synthetic"),
  c("T_synthetic_", "T_synthetic_total", "Synthetic Methods"),
  c("T_other_", "T_other_total", "Other Method")
)

# Dummy List
D.CACHE_T <- list()
# Loop through our Treatment Types
for(i in oList){
  # Get Columns which are starting with List value
  x <- grepl(i[1], colnames(D.FULL), fixed = TRUE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE_T[[i[2]]] <- rowSums(D.FULL[, x], na.rm = TRUE)
  # create a yes (1) no (2) list too
  xn <- paste( i[2], "_yn", sep = "")
  D.CACHE_T[[xn]] <- ifelse((rowSums(D.FULL[, x], na.rm = TRUE)) > 0, 1, 0)
}

# Convert List to Dataframe
D.CACHE_T <- data.frame(D.CACHE_T)
# sum rows by yn column, that way we get amount of different treatments used
x <- grep("_yn", colnames(D.CACHE_T), fixed = TRUE)
D.CACHE_T$T_amount <- rowSums(D.CACHE_T[, x], na.rm = TRUE)
D.FULL <- cbind(D.FULL, D.CACHE_T)

# Create a summary DF to create the BAR Plot
D.PLOT_T <- D.FULL %>%
  group_by( T_amount ) %>%
  summarise(
    n = n(),
    np = format( round( (n() / NROW( D.FULL$T_amount[( D.FULL$T_amount != 0 )]) * 100), 1), nsmall = 1 )
  )
# removed 0 ones, as we also dont count them in %
D.PLOT_T <- D.PLOT_T[(D.PLOT_T$T_amount != 0), ]
# change to factor otherwise we would need to use continious x axis
D.PLOT_T$T_amount <- as.factor( D.PLOT_T$T_amount )

# Create Box Plot Model with number of different treatment methods used as factor
CACHE.M_T <- F_EXTRACT_N( D.FULL, "T_amount", "Amount of diff. Treatments" )
CACHE.BIND_T <- F_GLM_FACTOR( D.FULL, "T_amount", get( "T_amount", pos = D.FULL ) )
CACHE.BIND_T <- cbind( CACHE.M_T, CACHE.BIND_T )
# remove n < 10
CACHE.BIND_T <- CACHE.BIND_T[(CACHE.BIND_T$n > 10), ]
# remove 0, this means participants said he uses treatment but did not say what treatment
CACHE.BIND_T <- CACHE.BIND_T[(CACHE.BIND_T$ff != 0), ]
CACHE.BIND_T$ff <- as.factor( CACHE.BIND_T$ff )

#### Combinations ####
D.COMB <- 
  setNames( 
    data.frame( matrix( ncol = 6, nrow = 0)), 
    c( "t", "c", "n", "lowerlim", "middle", "upperlim")
  )

# Get column number with *_yn in it
ColComb1 <- grep("_yn", colnames(D.FULL), fixed = TRUE)
# Calculate every possible combination
ColComb2 <- combn( ColComb1 , 2, simplify = FALSE )
ColComb3 <- combn( ColComb1 , 3, simplify = FALSE )
ColComb4 <- combn( ColComb1 , 4, simplify = FALSE )

# Add to our oList with the names the ColNumber for better inserting later
CacheList <- data.frame(t(sapply(oList, c)))
CacheList <- cbind(CacheList, ColComb1)

CACHE.COMB <- F_COMBINATION(D.FULL, ColComb1, 1, CacheList, ColComb1)
CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb2, 2, CacheList, ColComb1)
CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB1)
CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb3, 3, CacheList, ColComb1)
CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB1)
CACHE.COMB1 <- F_COMBINATION(D.FULL, ColComb4, 4, CacheList, ColComb1)
CACHE.COMB <- rbind(CACHE.COMB, CACHE.COMB1)

# D.COMB <- D.COMB[D.COMB$n != 0, ]

write.csv( D.COMB, file = paste("./", "Combination_Treatments.csv", sep = "" ) )


#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS.PLOT ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "bottom", scales = "free_x", ncol = 3  ) +
  xlab("") + ylab("Probability of loss [%]") + 
  #ggtitle("Loss prob. by operational factors") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    #strip.background = element_blank(),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
    ) +
  scale_x_discrete(
    # labels = paste( D.FACTORS.PLOT$ff,"\n ( n = ",D.FACTORS.PLOT$n, " )", sep="" )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
   # limits = c( 0, 25 )
  )

p2 <- ggplot( data = D.PLOT_T ) +
  aes( x = T_amount, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Number of diff. treatment types") + ylab("Number of beekeepers (n)") + 
  ggtitle("(D) Different treatment methods per company") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 750, 50 ),
    limits = c( 0, 600 )
  )

p3 <- 
  ggplot( CACHE.BIND_T, aes( x = ff, y = middle )) +
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_point() +
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 1.0 )+ 
  xlab("Number of diff. treatment types") + ylab("Probability of loss [%]") + 
  ggtitle("(E) Different treatment methods to probability of loss") +
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
    labels = paste( CACHE.BIND_T$ff," ( n = ",CACHE.BIND_T$n, " )", sep="" ),
    limits = c( levels( CACHE.BIND_T$ff ))
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
    #limits = c( 0, 10 )
  )


gtitle = textGrob( "Treatment Survey Data Overview" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ), c( 2, 3 ) )
p1 <- arrangeGrob( p1, p2, p3,
              top = gtitle, 
              layout_matrix = lay)
# Save File
ggsave("./img/Plot_Treatment_Overview.pdf", p1, width = 11, height = 8, units = "in")
