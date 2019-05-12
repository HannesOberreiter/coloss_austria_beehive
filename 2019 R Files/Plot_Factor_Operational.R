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
  c("no_foundation", "(H) No Foundation"),
  c("foreign_wax", "(G) Foreign wax"),
  c("migratory_beekeeper", "(B) Migration"),
  c("mash_bottom_board", "(F) Mash Bottom"),
  c("insulated_hives", "(E) Insulated Hives"),
  c("plastic_hives", "(D) Plastic Hives"),
  c("cert_org_beek", "(A) Cert. Organic"),
  c("varroatolerant", "(C) VSH Bees"),
  c("small_broodcells", "(I) Small Broodcells")
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
  # remove "Uncertain" or not ...
  #D.FULL_C <- subset( D.FULL, temp.n != "Unsicher" )
  D.FULL_C <- D.FULL
  
  CACHE.M <- F_EXTRACT_N( D.FULL_C, i[1], i[2] )
  CACHE.BIND <- F_GLM_FACTOR( D.FULL_C, i[1], get( i[1], pos = D.FULL_C ) )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Translate Factors into english for plotting
D.FACTORS.PLOT <- D.FACTORS
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Ja" ] <- "Yes"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Nein" ] <- "No"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Unsicher" ] <- "Uncertain"
# Remove Uncertain values with lower n 10
D.FACTORS.PLOT <- D.FACTORS.PLOT[!(D.FACTORS.PLOT$ff == "Uncertain" & D.FACTORS.PLOT$n < 10), ]

D.FACTORS.PLOT$ff <- factor( D.FACTORS.PLOT$ff, 
                             levels = c( "Yes", "No", "Uncertain"))

#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS.PLOT ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "bottom", scales = "free_x", ncol = 5  ) +
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
    breaks = seq( 0, 25, 5 )
    #limits = c( 0, 25 )
  )

gtitle = textGrob( "Loss prob. by operational factors" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ) )
p1 <- arrangeGrob(  p1,
              top = gtitle, 
              layout_matrix = lay)

ggsave("./img/Plot_Operational_Losses.pdf", p1, width = 11, height = 8, units = "in")

