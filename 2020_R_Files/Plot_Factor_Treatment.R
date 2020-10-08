##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### ALL SINGLE TREATMENT PLOTS ###########

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
# Remove participants which did not answer varroa_treated (this includes eg. beekeeping journal without given question)
D.FULL <- D.FULL[!is.na(D.FULL$varroa_treated),]
# Only select people which did treatment and give us at least some months
D.FULL <- D.FULL[D.FULL$varroa_treated == "Ja" & D.FULL$T_amount > 0,]

# Just test for queen related mortality
#D.FULL$hives_lost_e <- D.FULL$lost_a
#D.FULL$hives_spring_e <- D.FULL$hives_spring_queen

V.SEASONS <- c("spring", "summer", "winter")
V.COLORS  <- c("cornflowerblue", "forestgreen", "azure3")

# temporary list
D.FACTORS = list()

for(i in 2:nrow(treatmentList)){
  for(j in V.SEASONS){
    V.REGEX <- paste( treatmentList$ttotal[i], "yn_", j, sep = "")
    # Check if there is only "no", means this treatment was not used at this time, next jumps to next iternation
    if( length( unique( get( V.REGEX, pos = D.FULL ))) == 1) next
    CACHE.M <- F_EXTRACT_N( D.FULL, V.REGEX, treatmentList$tname[i] )
    # If there are not over 19 n we skip it
    if( (CACHE.M$n[CACHE.M$ff == "1"]) < 20) next
    # add season column to later split it
    CACHE.M$season <- j  
    CACHE.BIND <- F_GLM_FACTOR( D.FULL, V.REGEX, get( V.REGEX, pos = D.FULL ), TRUE )
    D.FACTORS[[paste(treatmentList$tname[i], j, sep="-")]] <- cbind( CACHE.M, CACHE.BIND )
  }
}
# Change List to tibble
D.FACTORS <- bind_rows(D.FACTORS)
# cleanup
rm(i, j, V.REGEX, CACHE.M, CACHE.BIND)
# Rename 1 and 0
D.FACTORS$ff <- ifelse(D.FACTORS$ff == 1, "Ja", "Nein")
# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = c( "Ja", "Nein"))

# Creating xAxisLetters and add it to DF
V.LETTERS <- c(LETTERS[1:26], paste0("A",LETTERS[1:26]))
D.FACTORS$letter <- ""
for(i in V.SEASONS){
  # logical vector for subsetting the different seasons in the dataframe
  V.SUB <- D.FACTORS$season == i
  # generate labels
  V.LENGTH <- length(unique(D.FACTORS$c[V.SUB]))
  V.LETTER_TEMP <- V.LETTERS[1:V.LENGTH]
  V.LETTER_TEMP <- c(V.LETTER_TEMP, V.LETTER_TEMP)
  V.LETTER_TEMP <- sort(V.LETTER_TEMP)
  # add labels to dataframe with name
  D.FACTORS$letter[V.SUB] <- paste("(",V.LETTER_TEMP,") ", D.FACTORS$c[V.SUB], sep = "")
}
D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)

# cleanup
rm(V.SUB, V.LENGTH, V.LETTER_TEMP, V.LETTERS, i)
L.PLOTS <- list()
for (i in 1:length(V.SEASONS)) {
  V.season = V.SEASONS[i]
  V.color <- V.COLORS[i]
  
  L.PLOTS[[V.season]] <- ggplot( data = D.FACTORS[D.FACTORS$season == V.season,] ) +
    aes( x = ff, y = middle ) + 
    geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = V.color) +
    geom_point(size = 3) + 
    geom_text(aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
    facet_wrap( ~ letter, strip.position = "top", scales = "free_x", ncol = 5  ) +
    xlab("") + ylab("Verlustrate [%]") + 
    theme_classic() + 
    theme(
      panel.spacing = unit( 1, "lines" ),
      strip.text.x = element_text(size = 10.5),
      strip.placement = "outside",
      plot.title = element_text(hjust = 0), 
      axis.title.x = element_text(colour = "black" ), 
      axis.title.y = element_text(colour = "black", size = 11 ), 
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
      axis.line = element_line( linetype = "solid", size = 0.5 ),
      panel.grid.major.y = element_line( colour = "grey" ),
      panel.grid.minor.y = element_line( colour = "grey" ),
      axis.text.y = element_text(angle = 0, size = 11)
    ) +
    scale_x_discrete(
    ) +
    scale_y_continuous(
      expand = c( 0 , 0 ),
      breaks = seq( 0, 100, 5 ),
      limits = c(0, max(D.FACTORS$upperlim)+5)
    )
  
  # generate chistar brackets temporary dataframe
  D.TEMP <- D.FACTORS[D.FACTORS$season == V.season & D.FACTORS$chistar == 1,]
  D.ANNOTATION <- F_CHISTAR_DF(D.TEMP, "Ja", "Nein", "letter")
  if(nrow(D.ANNOTATION)> 0){
    L.PLOTS[[V.season]] <- L.PLOTS[[V.season]] + geom_signif(data=D.ANNOTATION, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE, vjust = 0.5)
  }
}
#L.PLOTS[[1]]
#L.PLOTS[[2]]
#L.PLOTS[[3]]

ggsave("./img/plot_treatment_spring.pdf", L.PLOTS[[1]], width = 12, height = 4, units = "in")
ggsave("./img/plot_treatment_summer.pdf", L.PLOTS[[2]], width = 12, height = 8, units = "in")
ggsave("./img/plot_treatment_winter.pdf", L.PLOTS[[3]], width = 12, height = 4, units = "in")
