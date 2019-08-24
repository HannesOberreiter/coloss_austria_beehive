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
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####

#### Treatment Basic Factors ####
# List of Factors we want in our Plot
oList = list(
  c("varroa_checked", "(A) Did monitor varroa infestation level"),
  # only one person which did not treatmend agains varroa mites
  #c("varroa_treated", "(B) Did treatmend agains varroa mites"),
  c("crippled_bees", "(B) Amount of crippled bees observed")
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

# Ordering
OrderVector <- c( "Yes", "No", "Uncertain", "Often", "Seldom", "None", "Don't know")
D.FACTORS.PLOT$ff <- factor( D.FACTORS.PLOT$ff, levels = OrderVector )
D.FACTORS.PLOT <- D.FACTORS.PLOT[ order( factor( D.FACTORS.PLOT$ff, levels = OrderVector )),]

#### Treatment Types Overview #####

#D.FULL$T_amount <- D.FULL$T_amount_total
#summary(D.FULL$T_amount_total)

# Create a summary DF to create the BAR Plot
D.PLOT_T <- D.FULL %>%
  group_by( T_amount ) %>%
  summarise(
    n = n(),
    np = F_NUMBER_FORMAT( n() / NROW( D.FULL$T_amount[( D.FULL$T_amount != 0 )]) * 100)
  )
# removed 0 ones, as we also dont count them in %
D.PLOT_T <- D.PLOT_T[(D.PLOT_T$T_amount != 0), ]
# change to factor otherwise we would need to use continious x axis
D.PLOT_T$T_amount <- as.factor( D.PLOT_T$T_amount )

#### Varroa Count Histogramm #####
# Get Columns which are starting with List value
x <- grepl("(T_vcount_)", colnames(D.FULL), fixed = FALSE, perl = TRUE)
# sum the col values
VC_count <- colSums(D.FULL[  , x ], na.rm = TRUE)
VC_text <- c("Apr. 18", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Jan.", "Feb.", "Mar.", "Apr. 19")
VC_color <- c("cornflowerblue", "cornflowerblue", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "grey13", "grey13", "grey13", "cornflowerblue", "cornflowerblue", "cornflowerblue")

#### Cost Distribution ####
# our sequence
K.SEQ <- c(0, 1, 5, 10, 15, 20, 25, 30 , Inf )
# labels for sequence
L.SEQ <- c("0", "1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "> 31")
# remove nas
D.FULL.COST <- D.FULL$costs[!(is.na(D.FULL$costs))]
#shapiro.test(D.FULL.COST)

# cut with our sequence
D.FULL.COST <- cut( D.FULL.COST, K.SEQ, label = L.SEQ, include.lowest = TRUE)
# create df
D.COST.PLOT <- tibble(D.FULL.COST, .name_repair = ~ make.names("a"))
# summarize data and calculate percentage
D.COST.PLOT <- D.COST.PLOT %>% group_by(a) %>% 
  summarize( 
    n = n(), 
    np = F_NUMBER_FORMAT( n() / nrow(D.COST.PLOT) * 100 )
  )

#### PLOTTING #####
p1 <- ggplot( data = D.FACTORS.PLOT ) +
  aes( x = ff, y = middle ) + 
  geom_bar( colour = "black", alpha = 0, fill = "white", show.legend = FALSE, stat = "identity", linetype = "longdash" ) + 
  geom_pointrange( aes( ymin = lowerlim, ymax = upperlim ), size = 0.2 ) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "top", scales = "free_x", ncol = 3  ) +
  xlab("") + ylab("Loss rate [%]") + 
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle = 0, hjust = 0, size = 12),
    plot.title = element_text(hjust = 0.5, size = 10), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
    #panel.grid.minor.y = element_line( colour = "grey" )
    ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

p2 <- ggplot( data = D.PLOT_T ) +
  aes( x = T_amount, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Different treatment types (amount)") + ylab("Number of beekeepers (n)") + 
  ggtitle("(C) Amount of different treatment methods used") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 1000, 50 ),
    limits = c( 0, max(D.PLOT_T$n)+100 )
  )


p3 <- ggplot() +
  aes( x = VC_text, y = VC_count, fill = VC_color) + 
  geom_bar( colour = "black", alpha = 1, show.legend = FALSE, stat = "identity", linetype = "solid") + 
  xlab("Months 2018 - 2019") + ylab("Number of beekeepers (n)") + 
  ggtitle("(D) Month of monitoring varroa infestation level") +
  theme_classic() + 
  scale_fill_identity() + # as we use our own colors
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
    limits = c( VC_text )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 1000, 100 )
  )


gtitle = textGrob( "Treatment survey data overview" , gp=gpar( fontsize = 20 , face = "bold" ) )

lay <- rbind( c( 1 ), c( 2, 3 ) )
p <- arrangeGrob( p1, p2, p3,
              top = gtitle, 
              layout_matrix = lay)
# Save File
ggsave("./img/Plot_Treatment_Overview.pdf", p, width = 12, height = 8, units = "in")

p4 <- ggplot( data = D.COST.PLOT ) +
  aes( x = a, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 40, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  xlab("Costs per colony [Euro]") + ylab("Number of beekeepers (n)") + 
  ggtitle("Distribution of estimated treatment \n cost of materials per colony") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 1000, 50 ),
    limits = c( 0, max(D.COST.PLOT$n)+100 )
  )

ggsave("./img/Plot_Treatment_Cost.pdf", p4, width = 5, height = 4, units = "in")
