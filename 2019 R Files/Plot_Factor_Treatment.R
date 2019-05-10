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
D.CACHE <- list()
# Loop through our Treatment Types
for(i in oList){
  # Get Columns which are starting with List value
  x <- grepl(i[1], colnames(D.FULL), fixed = TRUE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE[[i[2]]] <- rowSums(D.FULL[, x], na.rm = TRUE)
  # create a yes no list too
  xn <- paste( i[2], "yn", sep = "")
  D.CACHE[[xn]] <- ifelse((rowSums(D.FULL[, x], na.rm = TRUE)) > 0, "Yes", "No")
}
# Convert List to Dataframe
D.CACHE <- data.frame(D.CACHE)
D.FULL <- cbind(D.FULL, D.CACHE)

# Just test for queen related mortality
# D.FULL$hives_lost_e <- D.FULL$lost_a
# D.FULL$hives_spring_e <- D.FULL$hives_spring_queen

#### Data PLOT Yes / NO ####

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
    )
# Loop through list and create for all factors CI
for( i in oList){
  xn <- paste( i[2], "yn", sep = "")
  CACHE.M <- F_EXTRACT_N( D.FULL, xn, i[3] )
  CACHE.BIND <- F_GLM_FACTOR( D.FULL, xn, get( xn, pos = D.FULL ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

D.FACTORS.PLOT1 <- D.FACTORS

#### Data PLOT Months ####

# Create dummy Dataframe, to insert rows later
D.FACTORS2 <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
  )
# Loop through list and create for all factors CI
for( i in oList){
  xn <- paste( i[2], "", sep = "")
  CACHE.M <- F_EXTRACT_N( D.FULL, xn, i[3] )
  CACHE.BIND <- F_GLM_FACTOR( D.FULL, xn, get( xn, pos = D.FULL ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS2 <- rbind( D.FACTORS2, CACHE.BIND )
}

D.FACTORS.PLOT2 <- subset(D.FACTORS2, D.FACTORS2$ff != 0)
D.FACTORS.PLOT2$ff <- as.factor(D.FACTORS.PLOT2$ff)

#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS.PLOT1 ) +
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
    breaks = seq( 0, 25, 5 ),
    limits = c( 0, 25 )
  )

p2 <- ggplot( data = D.FACTORS.PLOT2 ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~c , strip.position = "bottom", scales = "free_x", ncol = 3  ) +
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
     #labels = paste( D.FACTORS.PLOT2$ff,"\n ( n = ",D.FACTORS.PLOT2$n, " )", sep="" )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 ),
    limits = c( 0, 100 )
  )


gtitle = textGrob( "Loss prob. by treatment factors" , gp=gpar( fontsize = 20 , face = "bold" ) )
gtitle2 = textGrob( "Loss prob. by treatment factors and amount of moths used" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ) )
p1 <- arrangeGrob( p1,
              top = gtitle, 
              layout_matrix = lay)


p2 <- arrangeGrob( p2,
                    top = gtitle2, 
                    layout_matrix = lay)

ggsave("P_FACTOR_Treatment_P1.pdf", p1, width = 8.27, height = 11.69, units = "in")
ggsave("P_FACTOR_Treatment_P2.pdf", p2, width = 8.27, height = 11.69, units = "in")

