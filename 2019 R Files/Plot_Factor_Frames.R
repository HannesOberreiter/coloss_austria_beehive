##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### OLD FRAMES EXCHANGE FACTOR PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

# Create new Column to check if there is a difference between changed brood frames and not, later we also create the % Plot
D.FULL$new_frames_yn <- D.FULL$new_frames
D.FULL$new_frames_yn[D.FULL$new_frames_yn == "0%"] <- "No"
D.FULL$new_frames_yn[(D.FULL$new_frames_yn != "No" & D.FULL$new_frames_yn != "NA")] <- "Yes"
# There seems to be one 0 entrie remove it
unique(D.FULL$new_frames)
D.FULL <- D.FULL[D.FULL$new_frames != 0, ]

# List of Factors we want in our Plot
oList = list(
  c("new_frames", "(B) Relative amount replaced [%]"),
  c("new_frames_yn", "(A) Replaced old brood frames")
)

unique(D.FULL$new_frames)

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
D.FACTORS.PLOT$ff[ D.FACTORS.PLOT$ff == "mehr als 50%" ] <- "51-100%"

D.FACTORS.PLOT$ff <- factor( D.FACTORS.PLOT$ff, 
                             levels = c( "Yes", "No", "0%", "1-30%", "31-50%", "51-100%"))

#### PLOTTING #####
## this code will also create subplot with Yes / No
# p1 <- ggplot( data = D.FACTORS.PLOT ) +
#   aes( x = ff, y = middle ) + 
#   geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
#   geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
#   geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 2.5 ) +
#   facet_wrap( ~ c, strip.position = "bottom", scales = "free_x", ncol = 3  ) +
#   xlab("") + ylab("Loss rate [%]") + 
#   theme_classic() + 
#   theme(
#     panel.spacing = unit( 1, "lines" ),
#     #strip.background = element_blank(),
#     strip.placement = "outside",
#     plot.title = element_text(hjust = 0.5), 
#     axis.title.x = element_text(colour = "black" ), 
#     axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
#     axis.line = element_line( linetype = "solid" ),
#     panel.grid.major.y = element_line( colour = "grey" ),
#     panel.grid.minor.y = element_line( colour = "grey" )
#     ) +
#   scale_x_discrete(
#   ) +
#   scale_y_continuous(
#     expand = c( 0 , 0 ),
#     breaks = seq( 0, 100, 5 )
#   )



# Plot without Yes / No
p1 <- ggplot( data = D.FACTORS.PLOT[1:4,] ) +
  aes( x = ff, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  #geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  #geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 2.5 ) +
  xlab("") + ylab("Loss rate [%]") + 
  #ggtitle("Loss rate to amount of replaced old brood frames") +
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limit = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 45, 5 )
  )

ggsave("./img/Plot_Factor_Frames.pdf", p1, width = 5, height = 4, units = "in")

#gtitle = textGrob( "Loss rate to replaced old brood frames" , gp=gpar(face = "bold" ) )

#lay <- rbind( c( 1 ))
#p <- arrangeGrob(  p1,
#                    top = gtitle, 
#                    layout_matrix = lay)

#ggsave("./img/Plot_Factor_Frames.pdf", p, width = 5, height = 4, units = "in")

