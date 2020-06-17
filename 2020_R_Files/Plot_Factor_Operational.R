##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
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
D.FULL <- D.RAW

# List of Factors we want in our Plot

L.oList.english = list(
  c("op_no_foundation", "(H) Natural comb (without foundation)"),
  c("op_foreign_wax", "(G) Purchase wax from outside own operation"),
  c("op_migratory_beekeeper", "(B) Migration"),
  c("op_mash_bottom_board", "(F) Screened (mesh) bottom board in Winter"),
  c("op_insulated_hives", "(E) Insulated hives"),
  c("op_plastic_hives", "(D) Hives made from synthetic materials"),
  c("op_cert_org_beek", "(A) Certified organic beekeeping"),
  c("op_varroatolerant", "(C) Queens bred from Varroa tolerant/resistant stock"),
  c("op_small_broodcells", "(I) Small brood cell size (5.1 mm or less)"),
  c("colonies_merged", "(J) Colonies merged before winter")
)

L.oList.german = list(
  c("op_no_foundation", "(H) Naturwabenbau (ohne Mittelwand)"),
  c("op_foreign_wax", "(G) Kaufe Wachs zu (kein eigener Wachskreislauf)"),
  c("op_migratory_beekeeper", "(B) Wanderimker"),
  c("op_mash_bottom_board", "(F) Offener Gitterboden im Winter"),
  c("op_insulated_hives", "(E) Isolierte Beuten im Winter"),
  c("op_plastic_hives", "(D) Kunststoff-Beuten"),
  c("op_cert_org_beek", "(A) Zertifizierte Bio-Imkerei"),
  c("op_varroatolerant", "(C) Zucht auf Varroatoleranz"),
  c("op_small_broodcells", "(I) Kleine Brutzellen (5,1 mm oder weniger)"),
  c("colonies_merged", "(J) Vereinigung von Völkern")
)

# Temporary List
D.FACTORS = list()
D.FACTORS.TWO = list()

# Loop through list and create for all factors CI
for( i in L.oList.german){
  # Question is not in Newspaper
  if(i[1] == "colonies_merged"){
    D.TEMP <- D.FULL[D.FULL$submitted != "Zeitung",]
  } else {
    D.TEMP <- D.FULL
  }
  V.COL <- get( i[1], pos = D.TEMP )
  CACHE.M <- F_EXTRACT_N( D.TEMP, i[1], i[2], FALSE)
  CACHE.BIND <- F_GLM_FACTOR( D.TEMP, i[1], V.COL, TRUE, FALSE )
  D.FACTORS[[i[2]]] <- cbind( CACHE.M, CACHE.BIND )
  
  if(length(V.COL[V.COL == "Unsicher" & !is.na(V.COL)])<30){
    D.FULL_C <- D.TEMP[V.COL != "Unsicher",]
    V.COL_C <- get( i[1], pos = D.FULL_C )
  } else {
    D.FULL_C <- D.TEMP
    V.COL_C <- V.COL
  }
  
  V.COL <- get( i[1], pos = D.FULL_C )
  CACHE.M <- F_EXTRACT_N( D.FULL_C, i[1], i[2], TRUE)
  CACHE.BIND <- F_GLM_FACTOR( D.FULL_C, i[1], V.COL_C, TRUE, TRUE )
  D.FACTORS.TWO[[i[2]]] <- cbind( CACHE.M, CACHE.BIND )
  
}

rm(i, D.FULL_C, CACHE.M, CACHE.BIND, V.COL)

D.FACTORS <- bind_rows(D.FACTORS)
D.FACTORS.TWO <- bind_rows(D.FACTORS.TWO)

# Ordering
D.FACTORS$ff[is.na(D.FACTORS$ff)] <- "keine \n Angaben"
D.FACTORS$ff <- factor( D.FACTORS$ff, 
                             levels = c( "Ja", "Nein", "Unsicher", "keine \n Angaben"))

# Plot Loss Rates
p1 <- ggplot( data = D.FACTORS.TWO ) +
  aes( x = ff, y = middle ) + 
  geom_crossbar(aes( ymin = lowerlim, ymax = upperlim ), fill = "white") +
  geom_point(size = 3) + 
  geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 3 ) +
  #geom_text(data =  D.FACTORS.TWO[(D.FACTORS.TWO$chistar == 1 & D.FACTORS.TWO$ff == 'Ja'),], aes( x = ff, y = 20, label = "*"), angle = 0, vjust = 0, hjust = -3, color = "black", size = 8 ) +
  facet_wrap( ~ c, strip.position = "top", scales = "free_x", ncol = 5, labeller = label_wrap_gen(width=30)  ) +
  xlab("") + ylab("Verlustrate [%]") + 
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.text.x = element_text(size = 11),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.title.y = element_text(colour = "black", size = 11 ), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
    axis.line = element_line( linetype = "solid", size = 0.5 ),
    panel.grid.major.y = element_line( colour = "grey" ),
    panel.grid.minor.y = element_line( colour = "grey" ),
    axis.text.y = element_text(angle = 0, size = 11)
    #axis.line = element_blank(),
    #panel.border = element_rect( fill = NA, linetype = "solid", colour = "black", size = 1 )
    ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    limits = c(0, max(D.FACTORS.TWO$upperlim)*1.3),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

D.TEMP <- D.FACTORS.TWO[D.FACTORS.TWO$chistar == 1,]
# Manual changing of sign. brackets
D.TEMP_1 <- D.TEMP[D.TEMP$c != "(C) Zucht auf Varroatoleranz",]
D.TEMP_2 <- D.TEMP[D.TEMP$c == "(C) Zucht auf Varroatoleranz" | D.TEMP$c == "(I) Kleine Brutzellen (5,1 mm oder weniger)",]
D.ANNOTATION_1 <- F_CHISTAR_DF(D.TEMP_1, "Ja", "Nein", "c")
D.ANNOTATION_2 <- F_CHISTAR_DF(D.TEMP_2, "Ja", "Unsicher", "c")
D.ANNOTATION <- rbind(D.ANNOTATION_1, D.ANNOTATION_2)
if(nrow(D.ANNOTATION)> 0){
  p1 <- p1 + geom_signif(data=D.ANNOTATION, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE)
}
p1

D.FACTORS.TWO$latex <- F_LATEX_CONF(D.FACTORS.TWO)

ggsave("./img/plot_operational_loss.pdf", p1, width = 12, height = 8, units = "in")

# Plot Histo
p2 <- ggplot( data = D.FACTORS ) +
  aes( x = ff, y = n) + 
  geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
  geom_text( aes( label = paste(np, "%", sep = "" )), angle = 55, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  facet_wrap( ~ c, strip.position = "top", scales = "free_x", ncol = 5, labeller = label_wrap_gen(width=30)  ) +
  xlab("") + ylab("Anzahl [n]") + 
  theme_classic() + 
  theme(
    panel.spacing = unit( 1, "lines" ),
    strip.text.x = element_text(size = 11),
    strip.placement = "outside",
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.title.y = element_text(colour = "black", size = 11 ), 
    axis.text.x = element_text(angle = -55, hjust = 0, size = 11, face = "bold"),
    axis.line = element_line( linetype = "solid", size = 0.5 ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(angle = 0, size = 11)
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, max(D.FACTORS$n)*1.2, 100 ),
    limits = c( 0, max(D.FACTORS$n)*1.2 )
  )

ggsave("./img/plot_operational_hist.pdf", p2, width = 12, height = 8, units = "in")
 
