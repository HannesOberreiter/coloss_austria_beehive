##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OPTERATION FACTOR PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

# List of Factors we want in our Plot
oList = list(
  c("no_foundation", "(H) Natural comb (without foundation)"),
  c("foreign_wax", "(G) Purchase wax from outside own operation"),
  c("migratory_beekeeper", "(B) Migration"),
  c("mash_bottom_board", "(F) Screened (mesh) bottom board in Winter"),
  c("insulated_hives", "(E) Insulated hives"),
  c("plastic_hives", "(D) Hives made from synthetic materials"),
  c("cert_org_beek", "(A) Certified organic beekeeping"),
  c("varroatolerant", "(C) Queens bred from Varroa tolerant/resistant stock"),
  c("small_broodcells", "(I) Small brood cell size (5.1 mm or less)")
)

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 11, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim")
    )

# Loop through list and create for all factors CI
for( i in oList){
  D.FULL_C <- D.FULL
  CACHE.M <- F_EXTRACT_N( D.FULL_C, i[1], i[2] )
  CACHE.BIND <- F_GLM_FACTOR( D.FULL_C, i[1], get( i[1], pos = D.FULL_C ), TRUE )
  CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
  D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )
}

# Translate Factors into english for plotting
D.FACTORS.PLOT <- D.FACTORS
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Ja" ] <- "Yes"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Nein" ] <- "No"
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "Unsicher" ] <- "Uncertain"
# Remove Uncertain values with lower n 30
D.FACTORS.PLOT <- D.FACTORS.PLOT[!(D.FACTORS.PLOT$ff == "Uncertain" & D.FACTORS.PLOT$n < 30), ]

D.FACTORS.PLOT$ff <- factor( D.FACTORS.PLOT$ff, 
                             levels = c( "Yes", "No", "Uncertain"))

#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS.PLOT ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 2.5 ) +
  geom_text(data =  D.FACTORS.PLOT[(D.FACTORS.PLOT$chistar == 1 & D.FACTORS.PLOT$ff == 'Yes'),], aes( x = ff, y = 20, label = "*"), angle = 0, vjust = 0, hjust = -3, color = "black", size = 8 ) +
  facet_wrap( ~ c, strip.position = "top", scales = "free_x", ncol = 5, labeller = label_wrap_gen(width=30)  ) +
  xlab("") + ylab("Loss rate [%]") + 
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
    axis.line = element_line( linetype = "solid", size = 0.5 ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" ),
    axis.text.y = element_text(angle = 0, size = 10)
    #axis.line = element_blank(),
    #panel.border = element_rect( fill = NA, linetype = "solid", colour = "black", size = 1 )
    ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 25, 5 )
  )

#gtitle = textGrob( "Loss rate by operational factors" , gp=gpar(fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ) )
p <- arrangeGrob(  p1,
              #top = gtitle, 
              layout_matrix = lay)

ggsave("./img/Plot_Operational_Losses.pdf", p, width = 11, height = 6, units = "in")

