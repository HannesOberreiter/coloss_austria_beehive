### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# --- Populationsdynamik ----

rm(list=ls())

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Functions.r" )

# ----- START CODE -----

# years and increase over summer and loss over upcoming winter in percent
years      = c(2013, 2014, 2015, 2016, 2017, 2018, 2019)
increase   = c(24.4, 24.6, 44.4, 44.9, 35.2, 25.4)
loss       = c(12.0, 28.6, 7.6, 22.5, 11.6, 15.3)
# Start Value for calculating the populationsdynamics
start = 100

# calculate % for use with multiplying
p_i = increase/100
p_l = loss/100
len   = length(years)

# dummy frame
D_DATA <- tibble(
  year = numeric(),
  season = character(),
  value = numeric()
)

for(i in 1:len){
  # first year start = startvalue
  if(i == 1){
    D_DATA = D_DATA %>% add_row(
      year = years[i],
      season = 'Frühjahr',
      value = start
    )
  }
  if(i < len){
    start = start + start*p_i[i]
    D_DATA = D_DATA %>% add_row(
      year = years[i],
      season = 'Herbst',
      value = start
    )
    start = start - start*p_l[i]
    D_DATA = D_DATA %>% add_row(
      year = years[i+1],
      season = 'Frühjahr',
      value = start
    )
  }
}

# idu is used to plot the points on an x axis
D_DATA$idu <- as.numeric(row.names(D_DATA))
D_DATA$value <- round(D_DATA$value)

p <- ggplot(data = D_DATA) +
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
    limits = c( NA, max(D_DATA$idu)+0.5 ),
    breaks = seq(1.5,14,2),
    labels = years
    #labels= D_DATA$season
  ) +
  scale_y_continuous(
    expand = c( 0 , 0 ),
    breaks = seq( 0, max(D_DATA$value)+50, 50 ),
    limits = c( 0, max(D_DATA$value)+50 )
  )

p
ggsave("./img/plot_population.pdf", p, width = 6, height = 3.5, units = "in")
 
