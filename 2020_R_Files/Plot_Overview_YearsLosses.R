##############################
##############################
### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #
##############################
##############################

####### Overview of the yearly loss rates ###########

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Header.r" )
source( "Partials_Functions.r" )

# ---- Start code ----
D.FULL <- D.RAW
# get current year losses
AUSTRIA.BIND <- F_GLM_SINGLE( D.FULL )
# all the previous year losses and current year
D.PLOT <- tibble(
  year     = c("2007/08", "2008/09", "2009/10", "2010/11", "2011/12", "2012/13","2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20"),
  middle   = c(     13.3,       9.3,      14.7,      16.4,      25.9,      17.9,     12.8,      28.4,       8.1,      23.0,      11.8,     15.2, AUSTRIA.BIND[,2]),
  lowerlim = c(     13.3,       9.3,      14.7,      16.4,      25.9,      17.9,     11.7,      27.0,       7.4,      22.1,      11.1,     14.4, AUSTRIA.BIND[,1]),
  upperlim = c(     13.3,       9.3,      14.7,      16.4,      25.9,      17.9,     14.0,      29.9,       8.8,      24.0,      12.5,     16.1, AUSTRIA.BIND[,3]),
  n        = c(        0,         0,         0,         0,         0,         0,     1023,      1259,      1289,      1656,      1391,     1534,      nrow(D.FULL)),
  ncolonies= c(    16217,     18141,      7676,     13179,     32471,     19406,    18794,     22882,     23418,     43852,    28373,     33651, sum(D.FULL$hives_winter)),
)
p <- ggplot(
    D.PLOT, 
    aes( y = year, x = middle )) +
  geom_crossbar(
    aes( xmin = lowerlim, xmax = upperlim ), fill = "white") +
  geom_point(size = 3) + 
  geom_text(
    aes( y = year, x = 2.5, label = paste("(TN = ", n, "; VÖ = ", ncolonies, ")", sep = "")), 
    angle = 0, color = "black", size = 4, hjust = 0 ) +
  ylab("Winter / Jahr") + xlab("Verlustrate [%]") +
  theme_classic() + 
  theme(
    plot.title = element_text(size=20), 
    axis.title.x = element_text(colour = "black", size = 15 ), 
    axis.title.x.top = element_blank(), 
    
    axis.title.y = element_text(colour = "black", size = 15 ), 
    
    axis.text.x = element_text(angle = 0, size = 13, face = "bold"),
    axis.text.y = element_text(angle = 0, size = 13, face = "bold", margin = margin(0, -52, 0, 0)),
    
    axis.line = element_line( linetype = "solid" ),
    panel.grid.major.x = element_line( colour = "grey" ),
    panel.grid.minor.x = element_line( colour = "grey" )
  ) +
  scale_y_discrete(
  ) +
  scale_x_continuous(
    sec.axis = dup_axis(),
    limits = c(0, max(D.PLOT$upperlim)*1.1),
    expand = c( 0 , 0 ),
    breaks = seq( 0, 100, 5 )
  )

ggsave("./img/plot_years_losses.pdf", p, width = 12, height = 6.5, units = "in")
