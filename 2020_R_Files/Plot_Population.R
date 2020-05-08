### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# --- Populationsdynamik ----

rm(list=ls())

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Header.r")
source( "Partials_Functions.r" )
# ----- START CODE -----

#### Calculate net gain of new colonies over summer season
D.FULL <- D.RAW

D.PROD <- tibble(
    spring = D.FULL$hives_spring_before,
    summer_change = D.FULL$hives_winter - D.FULL$hives_spring_before,
    winter = D.FULL$hives_winter,
    winter_loss = D.FULL$hives_winter - D.FULL$hives_lost
)
# Drop rows without answer for spring number of colonies
D.PROD <- D.PROD[!is.na(D.PROD$spring),]

# calculte net change percentages
D.RETURN <- lapply(D.PROD, sum)
D.RETURN$summer_percentage <- D.RETURN$winter * 100 / D.RETURN$spring - 100
D.RETURN$winter_loss_percentage <- D.RETURN$winter_loss * 100 / D.RETURN$winter - 100
D.RETURN <- bind_rows(D.RETURN)

D.RETURN$summer_percentage      <- F_NUMBER_FORMAT(D.RETURN$summer_percentage)
D.RETURN$winter_loss_percentage <- F_NUMBER_FORMAT(D.RETURN$winter_loss_percentage)

print("#################")
print("#####RESULTS#####")
print(D.RETURN)
print("#################")

# Old logic when we tried to use bootstrap to generate a CI for the population
# sample.boot <- boot(D.PROD, F.SUM, R = 9999, t = 6)
# d = org data from bootstrap, i = indices from bootstrap, t = column to return (5 = summer percentage, 6 = winter percentage)
# F.SUM <- function(d, i, t = 1){
#   D.RETURN <- lapply(d[i,], sum)
#   D.RETURN$summer_percentage <- D.RETURN$winter * 100 / D.RETURN$spring - 100
#   D.RETURN$winter_loss_percentage <- D.RETURN$winter_loss * 100 / D.RETURN$winter - 100
#   # print("Returning following column:")
#   # print(names(D.RETURN)[t])
#   r <- D.RETURN[[t]]
#   # print(r)
#   return(r)
# }

##### Generate Plot from fixed values #####

# years and increase over summer and loss over upcoming winter in percent
# increase is over given year (spring - spring) and loss is for following spring next year.
# eg. 2013/2014 increase 24.4% from spring 2013 - winter 2013; loss 12.0% from winter 2013 - spring 2014
V.YEARS      = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
D.CONST <- tibble(
  V.INCREASE   = c(24.4, 24.6, 44.4, 44.9, 35.2, 25.4, D.RETURN$summer_percentage) / 100,
  V.LOSS       = c(12.0, 28.6, 7.6, 22.5, 11.6, 15.3, D.RETURN$winter_loss_percentage*-1) / 100
)
# Start Value for calculating the populationsdynamics
V.START = 100

for(i in 1:length(V.YEARS)){
  # first year start = startvalue
  if(i == 1){
    V.CALC = V.START
    D.POP = tibble(
      year = V.YEARS[i],
      season = 'Frühjahr',
      value = V.START
    )
  }
  if(i < length(V.YEARS)){
    V.CALC = V.CALC + V.CALC*D.CONST$V.INCREASE[i]
    D.POP = D.POP %>% add_row(
      year = V.YEARS[i],
      season = 'Herbst',
      value = V.CALC
    )
    V.CALC = V.CALC - V.CALC*D.CONST$V.LOSS[i]
    D.POP = D.POP %>% add_row(
      year = V.YEARS[i],
      season = 'Frühjahr',
      value = V.CALC
    )
  }
}

# Cleanup
rm(i, V.CALC)

# idu is used to plot the points on an x axis
D.POP$idu <- as.numeric(row.names(D.POP))
D.POP$value <- round(D.POP$value)

p <- ggplot(data = D.POP) +
  aes(x = idu, y = value) +
  geom_point(aes(colour = season), show.legend = FALSE, size = 5) + 
  geom_line(aes(group = 1)) +
  geom_text(aes(label = value), nudge_x = -0.3, hjust="right", show.legend = FALSE)+
  geom_text(aes(y = 5, label=season, colour = season), show.legend = FALSE, size = 2.5) +
  ylab("Entwicklung Völkerzahl \n (basierend auf 100 Völkern am Beginn)") + 
  xlab("Zeitverlauf über Jahre") +
  ggtitle("") +
  theme_classic() + 
  #annotate("segment", x = 0, xend = Inf, y = 10, yend = 10) +
  theme(
    plot.title = element_text(), 
    axis.title.x = element_text(colour = "black" ), 
    axis.text.x = element_text(angle = 0, size = 12, face = "bold"),
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.y = element_line( colour = "grey" )
  ) +
  scale_x_continuous(
    limits = c( NA, max(D.POP$idu)+0.5 ),
    breaks = seq(1.5,length(V.YEARS)*2,2),
    labels = V.YEARS
    #labels= D_DATA$season
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, max(D.POP$value)+50, 50 ),
    limits = c( 0, max(D.POP$value)+50 )
  )

ggsave("./img/plot_population.pdf", p, width = 6, height = 3.5, units = "in")
 
