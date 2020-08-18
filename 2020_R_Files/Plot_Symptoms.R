##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Overview of loss symptomps ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Import Header
source( "Partials_Header.r" )

# Import our Custom Functions
source( "Partials_Functions.r" )

#### START CODE #####
D.FULL <- D.RAW
# remove participants without losses, not relevant for symptome analysis
D.FULL <- D.FULL[D.FULL$hives_lost_e != 0,]
# lost_a = queen problems
# lost_b = elementar damange
# lost_c = "normal" losses
# number of symptomatic losses is in accordance with "normal" losses
#D.LOSS_CORRECT = D.FULL[D.FULL$lost_c == D.FULL$symp_total & D.FULL$symp_total > 0,]
# total loss is in accordance with "normal" losses
#D.LOSS_TOTAL = D.FULL[(D.FULL$lost_c + D.FULL$lost_a) == D.FULL$symp_total & D.FULL$symp_total > 0,]
# all symptom reports
D.LOSS_WRONG = D.FULL[D.FULL$symp_total > 0,]

# get total values
#V.total_lost_C = sum(D.LOSS_CORRECT$lost_c)
#V.total_lost_T = sum(D.LOSS_TOTAL$lost_c + D.LOSS_TOTAL$lost_a)
V.total_lost_W = sum(D.LOSS_WRONG$lost_c + D.LOSS_WRONG$lost_a)
# Calculate column values for different symptomes
# V.symptomes_C = c(
#   sum_a = sum(D.LOSS_CORRECT$symp_a),
#   sum_b = sum(D.LOSS_CORRECT$symp_b),
#   sum_c = sum(D.LOSS_CORRECT$symp_c),
#   sum_d = sum(D.LOSS_CORRECT$symp_d),
#   sum_e = sum(D.LOSS_CORRECT$symp_e)
# )

# V.symptomes_T = c(
#   sum_a = sum(D.LOSS_TOTAL$symp_a),
#   sum_b = sum(D.LOSS_TOTAL$symp_b),
#   sum_c = sum(D.LOSS_TOTAL$symp_c),
#   sum_d = sum(D.LOSS_TOTAL$symp_d),
#   sum_e = sum(D.LOSS_TOTAL$symp_e)
# )

V.symptomes_W = c(
  sum_a = sum(D.LOSS_WRONG$symp_a),
  sum_b = sum(D.LOSS_WRONG$symp_b),
  sum_c = sum(D.LOSS_WRONG$symp_c),
  sum_d = sum(D.LOSS_WRONG$symp_d),
  sum_e = sum(D.LOSS_WRONG$symp_e)
)

# generate percentage
#V.c_vector =  F_NUMBER_FORMAT(V.symptomes_C / V.total_lost_C * 100)
#V.ac_vector = F_NUMBER_FORMAT(V.symptomes_T / V.total_lost_T * 100)
V.w_vector =  F_NUMBER_FORMAT(V.symptomes_W / V.total_lost_W * 100)
# generate plotting data frame
D.PLOT_P = tibble(
  symptomes = c("a)", "b)", "c)", "d)", "e)"),
  #a  = V.c_vector,
  #ac = V.ac_vector,
  w  = V.w_vector
)
D.PLOT_T = tibble(
  symptomes = c("a)", "b)", "c)", "d)", "e)"),
  #a  = V.symptomes_C,
  #ac = V.symptomes_T,
  w  = V.symptomes_W
)
# label strings
#V.string_a  = paste("Symptome bezogen auf \n tote Völker (Völker ohne \n Königinnen Probleme) \n n =", nrow(D.LOSS_CORRECT), "Imker,", sum(V.symptomes_C), "Symptome \n", V.total_lost_C, "Völker verloren", sep = " ")
#V.string_ac = paste("Symptome bezogen auf \n Gesamtverlust (verlorene \n Völker und Völker mit \n Königinnen Problemen) \n n =", nrow(D.LOSS_TOTAL), "Imker", sum(V.symptomes_T), "Symptome \n", V.total_lost_T, "Völker verloren", sep = " ")
V.string_w  = paste("Alle Symptomnennungen \n (inkl. Mehrfachnennungen sowie unvollständige Angaben) \n n =", nrow(D.LOSS_WRONG), "Imker,", sum(V.symptomes_W), "Symptome,", V.total_lost_W, "Völker verloren", sep = " ")
# transform dataframe to long format
D.PLOT_P.LONG <- pivot_longer(D.PLOT_P, cols=-"symptomes", names_to ='variable', values_to = "value")
D.PLOT_T.LONG <- pivot_longer(D.PLOT_T, cols=-"symptomes", names_to ='variable', values_to = "totals")
D.PLOT <- left_join(D.PLOT_P.LONG, D.PLOT_T.LONG)
D.PLOT$variable <- as.character(D.PLOT$variable)
# Add long labels
D.PLOT$Gruppe <- ""
#D.PLOT$Gruppe[D.PLOT$variable == 'a']  = V.string_a
#D.PLOT$Gruppe[D.PLOT$variable == 'ac'] = V.string_ac
D.PLOT$Gruppe[D.PLOT$variable == 'w']  = V.string_w
# Ordering
#D.PLOT$Gruppe<- factor( D.PLOT$Gruppe, levels = c(V.string_a, V.string_ac, V.string_w))
D.PLOT$Gruppe<- factor( D.PLOT$Gruppe, levels = c(V.string_w))

p <- ggplot(D.PLOT, aes(symptomes, totals)) +  
  geom_bar( aes(fill = Gruppe), position = position_dodge(.9), 
            stat="identity", colour = "black", alpha = 1, show.legend = FALSE, linetype = "solid") + 
  xlab("Symptome") + ylab("Häufigkeit der berichteten Symptome [n]") + 
  #geom_text( 
  #  aes(fill = Gruppe, y = totals + 0.3, label = paste(value, "%", sep = "" )), 
  #  position = position_dodge(.9), angle = 55, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  geom_text( 
    aes(fill = Gruppe, y = totals + 0.3, label = paste(value, "%", sep = "" )), 
    color = "black", size = 3, vjust = -1 ) +
  scale_fill_grey(start = 0, end = 1) +  
  theme_classic() + 
  annotate(
    geom="text", x="c)", y=max(D.PLOT$totals)*0.8, label= V.string_w, color="black", hjust = "left") +
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0, size = 11, face = "bold"),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, max(D.PLOT$totals)*1.1, 100 ),
    limits = c( 0, max(D.PLOT$totals)*1.1 )
  )
p
ggsave("./img/plot_symptome.pdf", p, width = 10, height = 5, units = "in")
