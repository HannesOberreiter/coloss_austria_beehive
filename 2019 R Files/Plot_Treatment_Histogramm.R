##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

####### TREATMENT HISTOGRAMM PLOT ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

# List of our Months for plot x axis
VC_text <- c("Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Jan.")
# Color our bars to represent our spring, summer, winter treatments
VC_color <- c("cornflowerblue", "cornflowerblue", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "grey13", "grey13", "grey13")

# Create dummy Dataframe, to insert rows later
D.PLOT.HIST <- 
  setNames( 
    data.frame( matrix( ncol = 5, nrow = 0)), 
    c( "ff", "x", "y", "group", "vc_color" )
  )

# Loop over our treatment list to extract all treatments and their months of usage
for( i in fulltreatmentList){
  # regex string
  treatmentexp <- paste("(", i[1], ")0[1-9]|(", i[1], ")1[0]", sep = "")
  # get logical expression of columns with given regex
  x <- grepl(treatmentexp, colnames(D.FULL), fixed = FALSE, perl = TRUE)
  # sum the col values
  x_sums <- colSums(D.FULL[  , x ], na.rm = TRUE)
  # if synthetic we give them an other group as we combine them later
  x_group <- ifelse(grepl("T_synthetic", i[1]), 1, 0)
  # just small dataframe for easier binding after
  x_m1 <- data.frame(i[3], VC_text, x_sums, x_group, VC_color)
  # add rows to our dummy df
  D.PLOT.HIST <- rbind(D.PLOT.HIST, x_m1)
}
# set again names, probably can do it before and delete this code line
D.PLOT.HIST <- setNames(D.PLOT.HIST, c( "ff", "x", "y", "group", "vc_color" ))

# group synthetic methods into a dummy df
D.PLOST_HIST.CACHE <- D.PLOT.HIST[D.PLOT.HIST$group == 1, ] %>% group_by(x, vc_color) %>% 
  summarize(
  ff = "Synthetic methods",
  y = sum(y),
  group = 1
)
D.PLOST_HIST.CACHE <- D.PLOST_HIST.CACHE[c( "ff", "x", "y", "group", "vc_color" )]
# inset dummy df and remove other snythetics products
D.PLOT.HIST <- bind_rows(D.PLOT.HIST[D.PLOT.HIST$group == 0, ], D.PLOST_HIST.CACHE)
# create dummy list for our plots
D.PLOT_LIST = list()

# Used on the title for easier comparison in text
TitleLettersTemp <- LETTERS[1:27]
count <- 0

# Generate Vector with total Frequencies for Ordering inside Loop
sum_cat <- D.PLOT.HIST %>% group_by(ff) %>% summarize(y = sum(y))
sum_cat <- sum_cat[order(sum_cat$y, decreasing = TRUE),]

# Plotting via loop, using now treatmentList as this has combinded synthetics
for( i in sum_cat$ff){
  
  ylab.text <- ifelse((count %in% c(0, 4, 8)), "Number of beekeepers (n)", "")
  count <- count + 1
  
  # Title of plot
  title <- paste("(", TitleLettersTemp[count], ") - ", i, sep = "")
  loopPlot <- D.PLOT.HIST[D.PLOT.HIST$ff == i, ]
  loopPlot$t <- title
  # our plot
  p_cache <- ggplot(
    loopPlot, aes( x = x, y = y, fill = vc_color)) +
    geom_bar(colour = "black",show.legend = FALSE, stat = "identity", linetype = "solid") + 
    xlab("") + ylab(ylab.text) + 
    #ggtitle(title) +
    scale_fill_identity() + # as we use our own colors
    theme_classic() + 
    facet_wrap(. ~ t, strip.position = "top") +
    theme(
      plot.title = element_text(hjust = 0, size = 10), 
      strip.placement = "outside",
      axis.title.x = element_text(colour = "black" ), 
      axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
      #axis.line = element_line( linetype = "solid" ),
      panel.grid.major.y = element_line( colour = "grey" ),
      panel.grid.minor.y = element_line( colour = "grey" ),
      panel.border = element_rect( fill = NA, linetype = "solid", colour = "black", size = 1 ), 
      axis.line = element_blank()
    ) +
    scale_x_discrete(
      limits = c( VC_text )
    ) +
    scale_y_continuous(
      expand = c( 0 , 0 ),
      # this prevents decimal points happening
      labels = scales::number_format(accuracy = 1),
      #breaks = seq( 0, 1000, 100 )
      limits = c( 0, max(D.PLOT.HIST$y[D.PLOT.HIST$ff == i])+15 )
    )
  # save plot into list
  D.PLOT_LIST[[count]] <- p_cache
}

#gtitle = textGrob( "Treatment method histogram by months" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2, 3, 4 ), c( 5, 6, 7, 8 ), c( 9, 10, 11, 12 ))
p <- arrangeGrob( D.PLOT_LIST[[1]], D.PLOT_LIST[[2]], D.PLOT_LIST[[3]], D.PLOT_LIST[[4]], D.PLOT_LIST[[5]], D.PLOT_LIST[[6]], 
                   D.PLOT_LIST[[7]], D.PLOT_LIST[[8]], D.PLOT_LIST[[9]], D.PLOT_LIST[[10]], D.PLOT_LIST[[11]], D.PLOT_LIST[[12]],
                   #top = gtitle, 
                   layout_matrix = lay)

# Save File
ggsave("./img/Plot_Treatment_Histogramm.pdf", p, width = 12, height = 8, units = "in")
