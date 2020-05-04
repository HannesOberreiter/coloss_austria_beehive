##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
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
D.FULL <- D.RAW

# List of our Months for plot x axis
V.LABEL <- c("Apr.", "Mai", "Juni", "Juli", "Aug.", "Sept.", "Okt.", "Nov.", "Dez.", "Jan.", "Feb.", "März")
# Color our bars to represent our spring, summer, winter treatments
V.COLOR <- c("cornflowerblue", "cornflowerblue", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "azure3", "azure3", "azure3", "white", "white")

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 5, nrow = 0)), 
    c( "ff", "x", "y", "group", "vc_color" )
  )

# Loop over our treatment list to extract all treatments and their months of usage
for( i in fulltreatmentList){
  # regex string
  treatmentexp <- paste("(", i[1], ")0[1-9]|(", i[1], ")1[0-2]", sep = "")
  # get logical expression of columns with given regex
  x <- grepl(treatmentexp, colnames(D.FULL), fixed = FALSE, perl = TRUE)
  # sum the col values
  x_sums <- colSums(D.FULL[  , x ], na.rm = TRUE)
  # if synthetic we give them an other group as we combine them later
  x_group <- ifelse(grepl("T_synthetic", i[1]), 1, 0)
  # just small dataframe for easier binding after
  x_m1 <- data.frame("ff" = i[3], "x" = V.LABEL, "y" = x_sums, "group" = x_group, "vc_color" = V.COLOR)
  # add rows to our dummy df
  D.FACTORS <- rbind(D.FACTORS, x_m1, make.row.names = FALSE)
}

# cleanup
rm(x_m1, i, x, x_group, x_sums, xn)

# group synthetic methods into a dummy df
D.SYNTHETIC <- D.FACTORS[D.FACTORS$group == 1, ] %>% group_by(x, vc_color) %>% 
    summarize(
    ff = "Chemische Methoden(+)",
    y = sum(y),
    group = 1
  ) %>%
  arrange(factor( D.SYNTHETIC$x, levels = c( V.LABEL )))

# reorder cols
D.SYNTHETIC <- D.SYNTHETIC[c( "ff", "x", "y", "group", "vc_color" )]

# inset dummy df and remove other snythetics products
D.FACTORS <- bind_rows(D.FACTORS[D.FACTORS$group == 0, ], D.SYNTHETIC)

# cleanup 
rm(D.SYNTHETIC)

# create dummy list for our plots
L.PLOTS = list()

# Used on the title for easier comparison in text
TitleLettersTemp <- LETTERS[1:27]
count <- 0

# Generate Vector with total Frequencies for Ordering inside Loop
D.SUM <- D.FACTORS %>% group_by(ff) %>% summarize(y = sum(y))
D.SUM <- D.SUM[order(D.SUM$y, decreasing = TRUE),]

# Plotting via loop, using now treatmentList as this has combinded synthetics
for( i in D.SUM$ff){
  
  ylab.text <- ifelse((count %in% c(0, 4, 8)), "Anzahl Imker [n]", "")
  count <- count + 1
  
  # Title of plot
  title <- paste("(", TitleLettersTemp[count], ") ", i, sep = "")
  loopPlot <- D.FACTORS[D.FACTORS$ff == i, ]
  loopPlot$t <- title
  # our plot
  p_cache <- ggplot(
    loopPlot, aes( x = x, y = y, fill = V.COLOR)) +
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
      limits = c( V.LABEL )
    ) +
    scale_y_continuous(
      expand = c( 0 , 0 ),
      # this prevents decimal points happening
      labels = scales::number_format(accuracy = 1),
      #breaks = seq( 0, 1000, 100 )
      limits = c( 0, max(D.FACTORS$y[D.FACTORS$ff == i])+15 )
    )
  # save plot into list
  L.PLOTS[[count]] <- p_cache
}

#gtitle = textGrob( "Treatment method histogram by months" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1, 2, 3, 4 ), c( 5, 6, 7, 8 ), c( 9, 10, 11, 12 ))
p <- arrangeGrob( L.PLOTS[[1]], L.PLOTS[[2]], L.PLOTS[[3]], L.PLOTS[[4]], L.PLOTS[[5]], L.PLOTS[[6]], 
                  L.PLOTS[[7]], L.PLOTS[[8]], L.PLOTS[[9]], L.PLOTS[[10]], L.PLOTS[[11]], L.PLOTS[[12]],
                   #top = gtitle, 
                   layout_matrix = lay)

# Save File
ggsave("./img/plot_treatment_histogramm.pdf", p, width = 12, height = 8, units = "in")

# cleanup
#rm(count, i, title, TitleLettersTemp, treatmentexp, V.COLOR, V.LABEL, ylab.text, D.CACHE, loopPlot, p_cache)
