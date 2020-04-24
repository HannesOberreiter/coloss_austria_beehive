##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Varroa Monitoring related graphs ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
#source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW

#### YES / NO PLOT #####

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 12, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim", "chi")
  )

CACHE.M <- F_EXTRACT_N( D.FULL, "varroa_checked", "varroa_checked" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "varroa_checked", get( "varroa_checked", pos = D.FULL), TRUE )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

rm(CACHE.BIND, CACHE.M)

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = c("Ja", "Nein", "Unsicher"))

p1 <- F_SINGLE_PLOT(D.FACTORS)
ggsave("./img/plot_treatment_varroa_checked.pdf", p1, width = 5, height = 4, units = "in")

rm(D.FACTORS)

#### VARROA Monitor Histogramm ####
# Get Columns which are starting with List value

V.LOGICAL <- grepl("(T_vcount)", colnames(D.FULL), fixed = FALSE, perl = TRUE)
V.COUNT <- colSums(D.FULL[  , V.LOGICAL ], na.rm = TRUE)
V.PERCENT <- V.COUNT / nrow(D.FULL[(D.FULL$varroa_checked == "Ja" & !is.na(D.FULL$varroa_checked)),]) * 100
V.PERCENT <- round(V.PERCENT, 1)

# Text String and colour
V.MONTHS <- c("Apr. 19", "Mai", "Juni", "July", "Aug.", "Sept.", "Okt.", "Nov.", "Dez.", "Jan. 20", "Feb.", "Mär.")
V.MONTHS.COLOR <- c("cornflowerblue", "cornflowerblue", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "forestgreen", "grey13", "grey13", "grey13", "cornflowerblue", "cornflowerblue")

p2 <- ggplot() +
  aes( x = V.MONTHS, y = V.COUNT, fill = V.MONTHS.COLOR) + 
  geom_text( aes( label = paste(V.PERCENT, "%", sep = "" )), angle = 60, vjust = 0, hjust = -0.1, color = "black", size = 3 ) +
  geom_bar( colour = "black", alpha = 1, show.legend = FALSE, stat = "identity", linetype = "solid") + 
  xlab("") + ylab("Anzahl Imker [n]") + 
  theme_classic() + 
  scale_fill_identity() + # as we use our own colors
  theme(
    plot.title = element_text(hjust = 0, size = 12), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 9, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 10),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
    limits = c( V.MONTHS )
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    limits = c(0, max(V.COUNT + 200)),
    breaks = seq( 0, 1000, 100 )
  )

ggsave("./img/plot_treatment_varroa_overview.pdf", p2, width = 6, height = 3.5, units = "in")

# cleanup
rm(V.COUNT, V.LOGICAL, V.MONTHS, V.MONTHS.COLOR)

#### Multiple Months ####
V.LABELS <- c("0 Monate", "1-3 Monate", ">3 Monate")

# generating columns with sums
V.LOGICAL <- grepl("(T_vcount)", colnames(D.FULL), fixed = FALSE, perl = TRUE)
D.FULL$monitoring_total <- rowSums(D.FULL[, V.LOGICAL], na.rm = TRUE)
D.FULL$monitoring_groups <- V.LABELS[1]
D.FULL$monitoring_groups[D.FULL$monitoring_total > 1] <- V.LABELS[2]
D.FULL$monitoring_groups[D.FULL$monitoring_total > 3] <- V.LABELS[3]

# Create dummy Dataframe, to insert rows later
D.FACTORS <- 
  setNames( 
    data.frame( matrix( ncol = 12, nrow = 0)), 
    c( "ff", "c", "n", "hives_winter", "lost_a", "lost_b", "lost_c", "hives_lost_rate", "lowerlim", "middle", "upperlim", "chi")
  )

CACHE.M <- F_EXTRACT_N( D.FULL, "monitoring_groups", "monitoring_groups" )
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "monitoring_groups", get( "monitoring_groups", pos = D.FULL), TRUE )
CACHE.BIND <- cbind( CACHE.M, CACHE.BIND )
D.FACTORS <- rbind( D.FACTORS, CACHE.BIND )

rm(CACHE.BIND, CACHE.M)

# Ordering
D.FACTORS$ff <- factor( D.FACTORS$ff, levels = V.LABELS)

p3 <- F_SINGLE_PLOT(D.FACTORS)

ggsave("./img/plot_treatment_varroa_grouped.pdf", p3, width = 5, height = 4, units = "in")

rm(D.FACTORS)
