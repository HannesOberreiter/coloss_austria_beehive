##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### TREATMENT METHODS COMBINATIONS PLOT ###########

# Set Working directory (uses API of RStudio)
#SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
#setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

# # Custom Function
# F_COMB_LIST <- function( x, d, itn, CacheList, ColComb1, negative = 1 ){
#   
#   D.LABEL <- tibble()
#   D.COMB <-list()
#   counter <- 1
#   countermax <- length(d)
#   
#   for(i in d){
#     # Progress Bar
#     counter <- counter + 1
#     # save col numbers to a second variable, we use later
#     d1 <- ColComb1
#     # save our df to our cache variable
#     CACHE.COMB <- x
#     # temporary colnames, to check later if values are ordered to correct treatments in df
#     coln <- ""
#     # reset our name fields
#     cname = ""
#     sname = ""
#     shortname = ""
#     # loop for multiple combination, single treatment method is only single run
#     for( n in 1:itn ){
#       # remove selected treatment(s) from the list of treatment columns
#       d1 <- d1 [ d1 != i[ n ] ]
#       # subsetting our dataframe, were only our selected treatments are given
#       CACHE.COMB <- CACHE.COMB[ CACHE.COMB[, i[n]] == negative , ]
#       coln <- paste( coln, colnames(CACHE.COMB[i[n]]), sep = " ")
#       # Names for Export
#       cname[n] = as.character( CacheList$X3[ CacheList$V.ColComb1 == i[n] ])
#       sname[n] = as.character( CacheList$X2[ CacheList$V.ColComb1 == i[n] ])
#       
#       shortname <- ifelse(n == 1,
#                           paste(CacheList$X4[ CacheList$V.ColComb1 == i[n] ]),
#                           paste(shortname, CacheList$X4[ CacheList$V.ColComb1 == i[n]], sep = "&" )
#       )
#       
#     }
#     
#     if( nrow( CACHE.COMB ) == 0 ) next
#     
#     if(negative == 1){
#       # count numbers without given treatment, if it is bigger than 0 it means there are other treatments in combination
#       # drop = FALSE tocreate a one-column dataframe and no vector if there is only one column left
#       #print(d1)
#       CACHE.COMB$comb_count <- rowSums(CACHE.COMB[ , d1, drop = FALSE ], na.rm = TRUE)
#       #print(CACHE.COMB$comb_count)
#       # only get the count 0 ones, because then we are sure there is no combination
#       CACHE.COMB <- CACHE.COMB[CACHE.COMB[, "comb_count"] == 0, ]
#     }
#     
#     if( nrow( CACHE.COMB ) == 0 ) next
#     
#     
#     CACHE.COMB$short <- shortname
#     
#     D.LABEL = bind_rows(D.LABEL, CACHE.COMB)
#     #D.LABEL$treatment[D.LABEL$id %in% CACHE.COMB$id] <- shortname
#   }
#   
#   D.LABEL <- D.LABEL[,c("id", "short")]
#   D.LABEL$t <- itn
#   
#   return(D.LABEL)
#   
# }
# 
# D.FULL <- D.RAW
# 
# V.oList = list(
#   
#   c("A", "FRÜHJAHR", "Hyperthermie",                 "F-Hyp. "),
#   c("A", "FRÜHJAHR", "Andere biotechnische Methode", "F-Biot. "),
#   c("A", "FRÜHJAHR", "Ameisensäure - Kurzzeit",      "F-AS-KZ "),
#   c("A", "FRÜHJAHR", "Ameisensäure - Langzeit",      "F-AS-LZ "),
#   c("A", "FRÜHJAHR", "Milchsäure",                   "F-Milchs. "),
#   c("A", "FRÜHJAHR", "Oxalsäure - sub.",             "F-Ox-Sub. "),
#   c("A", "FRÜHJAHR", "Oxalsäure - träufeln",         "F-Ox-Träu. "),
#   c("A", "FRÜHJAHR", "Thymol",                       "F-Thy "),
#   c("A", "FRÜHJAHR", "Anderes chem. Produkt",        "F-chem. Pr. "),
#   c("A", "FRÜHJAHR", "Andere Methode",               "F-Andere "),
#   
#   c("B", "SOMMER", "Hyperthermie",                 "S-Hyp."),
#   c("B", "SOMMER", "Andere biotechnische Methode", "S-Biot. "),
#   c("B", "SOMMER", "Ameisensäure - Kurzzeit",      "S-AS-KZ "),
#   c("B", "SOMMER", "Ameisensäure - Langzeit",      "S-AS-LZ "),
#   c("B", "SOMMER", "Milchsäure",                   "S-Milchs. "),
#   c("B", "SOMMER", "Oxalsäure - sub.",             "S-Ox-Sub. "),
#   c("B", "SOMMER", "Oxalsäure - träufeln",         "S-Ox-Träu. "),
#   c("B", "SOMMER", "Thymol",                       "S-Thy. "),
#   c("B", "SOMMER", "Anderes chem. Produkt",        "S-chem. Pr. "),
#   c("B", "SOMMER", "Andere Methode",               "S-Andere "),
#   
#   c("C", "WINTER", "Hyperthermie",                 "W-Hyp. "),
#   c("C", "WINTER", "Andere biotechnische Methode", "W-Biot. "),
#   c("C", "WINTER", "Ameisensäure - Kurzzeit",      "W-AS-KZ "),
#   c("C", "WINTER", "Ameisensäure - Langzeit",      "W-AS-LZ "),
#   c("C", "WINTER", "Milchsäure",                   "W-Milchs. "),
#   c("C", "WINTER", "Oxalsäure - sub.",             "W-Ox-Sub. "),
#   c("C", "WINTER", "Oxalsäure - träufeln",         "W-Ox-Träu. "),
#   c("C", "WINTER", "Thymol",                       "W-Thy. "),
#   c("C", "WINTER", "Anderes chem. Produkt",        "W-chem. Pr. "),
#   c("C", "WINTER", "Andere Methode",               "W-Andere ")
#   
# )
# 
# 
# D.FULL <- D.FULL[, -grep("T_drone", colnames(D.FULL))]
# D.FULL <- D.FULL[, -grep("T_vcount", colnames(D.FULL))]

# TODO don't remove columns otherwise participants with some combination will not be included into other combination even if it is not true
# Remove other columns which are low represented and would only blow our combinations
#D.FULL <- D.FULL[, -grep("T_formic_short_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_formic_long_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_formic_short_totalyn_spring", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_formic_long_totalyn_spring", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_biotechnical_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_thymol_totalyn_spring", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_thymol_totalyn_winter", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_synthetic", colnames(D.FULL))]
#D.FULL <- D.FULL[, -grep("T_other", colnames(D.FULL))]

# Get column number with yn_* in it
# V.ColComb1  <- grep("totalyn_", colnames(D.FULL), fixed = TRUE)
# # Add to our oList with the names the ColNumber for better inserting later
# V.CacheList <- data.frame(t(sapply(V.oList, c)))
# rm(V.oList)
# V.CacheList <- cbind(V.CacheList, V.ColComb1)
# 
# 
# T_LIST1 <- F_COMB_LIST(D.FULL, V.ColComb1, 1, V.CacheList, V.ColComb1)
# V.ColCombN <- combn( V.ColComb1 , 2, simplify = FALSE )
# T_LIST2 <- F_COMB_LIST(D.FULL, V.ColCombN, 2, V.CacheList, V.ColComb1)
# V.ColCombN <- combn( V.ColComb1 , 3, simplify = FALSE )
# T_LIST3 <- F_COMB_LIST(D.FULL, V.ColCombN, 3, V.CacheList, V.ColComb1)
# 
# T_LIST <- bind_rows(T_LIST1, T_LIST2, T_LIST3)
# 
# D.FULL <- left_join(D.FULL, T_LIST)

D.FULL = D.RAW
# Remove Drohnebrood Removal
D.FULL$short = D.FULL$c_short
D.FULL$short = str_replace_all(D.FULL$short, "F-Drohne &", "")
D.FULL$short = str_replace_all(D.FULL$short, "S-Drohne &", "")
D.FULL$short = str_replace_all(D.FULL$short, "W-Drohne &", "")
D.FULL$short = str_trim(D.FULL$short)

D.FACTORS <- F_EXTRACT_N(D.FULL, "short", "short", TRUE)
CACHE.BIND <- F_GLM_FACTOR( D.FULL, "short", D.FULL$short, TRUE)
D.FACTORS <- cbind( D.FACTORS, CACHE.BIND )

D.FACTORS$t = str_count(D.FACTORS$ff, pattern = "&") + 1

#### VERTICAL LOSS PLOT ####
V.threecolors       <- c("white", "gray", "black")
V.threetextcolors   <- c("black", "black", "white")
V.shapeLetters      <- c(65:79, 80:90)

D.FACTORS$circle_color <- V.threecolors[D.FACTORS$t]
D.FACTORS$text_color   <- V.threetextcolors[D.FACTORS$t]
D.FACTORS$t            <- as.factor(D.FACTORS$t)
D.FACTORS$short        <- str_replace_all(D.FACTORS$ff, "&", "& ")

# Austria Total Loss Rate
AUSTRIA.BIND <- F_GLM_SINGLE( D.RAW )

D.FACTORS <- D.FACTORS[order(D.FACTORS$n, decreasing = TRUE),]
D.FACTORS$short <- factor(D.FACTORS$short, levels = D.FACTORS$short)
D.FACTORS$letters <- V.shapeLetters[1:nrow(D.FACTORS)]
D.FACTORS$latex <- F_LATEX_CONF(D.FACTORS)

PLOT.LOSS <- 
  ggplot(
    D.FACTORS[D.FACTORS$n > 14,], 
    aes( y = short, x = middle, shape = short)) +
  geom_crossbar(
    aes( xmin = lowerlim, xmax = upperlim ), fill = "white") +
  geom_vline(xintercept = AUSTRIA.BIND[1], linetype="dashed", color = "red", size=0.5) +
  geom_vline(xintercept = AUSTRIA.BIND[2], color = "red", size=0.5) + 
  geom_vline(xintercept = AUSTRIA.BIND[3], linetype="dashed", color = "red", size=0.5) +
  geom_point(
    aes(fill = t), 
    shape = 21, size = 7, color = "black", show.legend = FALSE) + 
  geom_point(
    aes(colour = t), 
    size = 3, stroke = 1, show.legend = FALSE) + 
  scale_shape_manual( values = V.shapeLetters[1:20] ) +
  scale_fill_manual(  values = V.threecolors) +
  scale_colour_manual(values = V.threetextcolors) +
  geom_text(
    aes( y = short, x = 2, label = paste("n = ", n )), 
    angle = 0, color = "black", size = 3 ) +
  #ggtitle("Combination of treatment methods loss rates") +
  ylab("") + xlab("Verlustrate [%]") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=20), 
    axis.title.x = element_text(colour = "black", size = 15 ), 
    axis.title.y = element_text(colour = "black", size = 15 ), 
    
    axis.text.x = element_text(angle = 0, size = 13, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 13, face = "bold"),
    
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.x = element_line( colour = "grey" ),
    #panel.grid.minor.x = element_line( colour = "grey" )
  ) +
  scale_y_discrete(
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

PLOT.LOSS

knitr::kable(D.FACTORS[D.FACTORS$n > 14,c(1,3,18)], format="latex", booktabs=T)

write_excel_csv2(D.FACTORS, "comb_new.csv")

D.FACTORS <- D.FACTORS[D.FACTORS$n > 14,]
paste(D.FACTORS$middle, " (", D.FACTORS$lowerlim, "-", D.FACTORS$upperlim, ")", sep="")

ggsave("./img/plot_combination_loss_new.pdf", PLOT.LOSS, width = 12, height = 6.5, units = "in")


