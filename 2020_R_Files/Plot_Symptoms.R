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

# lost_a = queen problems
# lost_b = elementar damange
# lost_c = "normal" losses

# number of symptomatic losses is in accordance with "normal" losses
D.LOSS_CORRECT = D.FULL[D.FULL$lost_c == D.FULL$symp_total,]
# total loss is in accordance with "normal" losses
D.LOSS_TOTAL = D.FULL[(D.FULL$lost_c + D.FULL$lost_a) == D.FULL$symp_total,]
# all symptom reports
D.LOSS_WRONG = D.FULL[D.FULL$symp_total > 0,]

total_lost_C = sum(D.LOSS_CORRECT$lost_c)
total_lost_T = sum(D.LOSS_TOTAL$lost_c + D.LOSS_TOTAL$lost_a)
total_lost_W = sum(D.LOSS_WRONG$lost_c + D.LOSS_WRONG$lost_a)

symptomes_C = c(
  sum_a = sum(D.LOSS_CORRECT$symp_a),
  sum_b = sum(D.LOSS_CORRECT$symp_b),
  sum_c = sum(D.LOSS_CORRECT$symp_c),
  sum_d = sum(D.LOSS_CORRECT$symp_d),
  sum_e = sum(D.LOSS_CORRECT$symp_e)
)

symptomes_T = c(
  sum_a = sum(D.LOSS_TOTAL$symp_a),
  sum_b = sum(D.LOSS_TOTAL$symp_b),
  sum_c = sum(D.LOSS_TOTAL$symp_c),
  sum_d = sum(D.LOSS_TOTAL$symp_d),
  sum_e = sum(D.LOSS_TOTAL$symp_e)
)

symptomes_W = c(
  sum_a = sum(D.LOSS_WRONG$symp_a),
  sum_b = sum(D.LOSS_WRONG$symp_b),
  sum_c = sum(D.LOSS_WRONG$symp_c),
  sum_d = sum(D.LOSS_WRONG$symp_d),
  sum_e = sum(D.LOSS_WRONG$symp_e)
)

c_vector = F_NUMBER_FORMAT(symptomes_C / total_lost_C * 100)
ac_vector = F_NUMBER_FORMAT(symptomes_T / total_lost_T * 100)
w_vector = F_NUMBER_FORMAT(symptomes_W / total_lost_W * 100)

p = data.frame(
  symptomes = c("a)", "b)", "c)", "d)", "e)"),
  a = c_vector,
  ac = ac_vector,
  w = w_vector
)

string_a = paste("Symptome bezogen auf \n tote Völker (Völker ohne \n Königinnen Probleme) \n n =", nrow(D.LOSS_CORRECT), "Imker,", total_lost_C, "Völker", sep = " ")
string_ac = paste("Symptome bezogen auf \n Gesamtverlust (verlorene \n Völker und Völker mit \n Königinnen Problemen) \n n =", nrow(D.LOSS_TOTAL), "Imker", total_lost_T, "Völker", sep = " ")
string_w = paste("Alle Symptomnennungen \n (inkl. Mehrfachnennungen \n sowie unvollständige Angaben) \n n =", nrow(D.LOSS_WRONG), "Imker,", total_lost_W, "Völker", sep = " ")

p.m <- melt(p, id.vars='symptomes')
p.m$variable <- as.character(p.m$variable)

p.m$variable[p.m$variable == 'a'] = string_a
p.m$variable[p.m$variable == 'ac'] = string_ac
p.m$variable[p.m$variable == 'w'] = string_w


p.m$variable<- factor( p.m$variable, levels = c(string_a, string_ac, string_w))

names(p.m)[2] <- "Gruppe"

# cleanup
# rm(CACHE.M, CACHE.BIND)

p <- ggplot(p.m, aes(symptomes, value)) +  
  geom_bar( aes(fill = Gruppe), position = position_dodge(.9), stat="identity", colour = "black", alpha = 1, show.legend = TRUE, linetype = "solid") + 
  xlab("Symptome") + ylab("Angaben der Imker [%]") + 
  geom_text( 
    aes(fill = Gruppe, y = value + 0.3, label = paste(value, "%", sep = "" )), 
    position = position_dodge(.9), angle = 55, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
  scale_fill_grey(start = 0, end = 1) +  
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, hjust = 0, size = 8, face = "bold"),
    axis.line = element_line( linetype = "solid" )
  ) +
  scale_x_discrete(
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 10 ),
    limits = c( 0, max(p.m$value)+10 )
  )

ggsave("./img/plot_symptome.pdf", p, width = 5, height = 4, units = "in")

