##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

# Our Custom Functions

# GLM with Factors
# x = Dataframe, f = factor as string, xf is factor from dataframe
##############################
F_GLM_FACTOR <- function( x, f, xf ){
  x$ff <- get( f, pos = x ) 
  #print(x$ff)
  GLM.FULL <- glm( 
    cbind( hives_lost_e, hives_spring_e ) ~ as.factor( ff ) , # F*** it, we need to use "Bundesland" here
    family = quasibinomial( link = "logit" ), 
    data = x, na.action = na.omit )
  SUMMARY.FULL <- summary( GLM.FULL )
  print( SUMMARY.FULL ) 
  # Check which DF we should use
  ANOVA.FULL <- anova( GLM.FULL, test = "F" )
  print( paste( "ANOVA TEST --> ", f ))
  print( ANOVA.FULL ) 
  write.csv( ANOVA.FULL, file = paste(f, "_ANOVA.csv", sep = "" ) )
  
  # Here we use the better one
  DFRESIDUAL.FULL <- SUMMARY.FULL$df.residual
  VALUES.FULL <- predict( GLM.FULL, data.frame( ff = levels( as.factor(xf) ) ), type = "link", se.fit = T )
  
  # Approximate 95% CIs for the log odds & upper and lower limit (NOT WITH REFERENCE REGION, Dunno)
  CACHE.ODDS <- VALUES.FULL$fit
  CACHE.LOWERLIM <- VALUES.FULL$fit - qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  CACHE.UPPERLIM <- VALUES.FULL$fit + qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  # Add Prob. to our plot matrix
  CACHE.ODDS = inv.logit( CACHE.ODDS ) * 100
  CACHE.LOWERLIM = inv.logit( CACHE.LOWERLIM ) * 100
  CACHE.UPPERLIM = inv.logit( CACHE.UPPERLIM ) * 100
  
  return( CACHE.BIND <- cbind( lowerlim = CACHE.LOWERLIM, middle = CACHE.ODDS, upperlim = CACHE.UPPERLIM ) )
}

# Extract our N into a DF
# x = Dataframe, f = level, c = Name for Factor
##############################
F_EXTRACT_N <- function( x, f, c ){
  x$ff = get( f, pos = x ) 
  D.CACHE <- x %>% 
    group_by( ff ) %>% 
    summarize(
      c = c,
      n = n(),
      hives_winter = sum(hives_winter),
      lost_a = sum(lost_a),
      lost_b = sum(lost_b),
      lost_c = sum(lost_c),
      hives_lost_rate = sum( hives_lost_e ) / sum( hives_winter ) * 100
    )
  print( D.CACHE )
  D.CACHE <- D.CACHE %>% na.omit()
  return( D.CACHE )
}

F_GLM_RETURN <- function( x, f, xf ){
  x$ff = get( f, pos = x ) 
  print(x$ff)
  GLM.FULL <- glm( 
    cbind( hives_lost_e, hives_spring_e ) ~ as.factor( ff ) , # F*** it, we need to use "Bundesland" here
    family = quasibinomial( link = "logit" ), 
    data = x, na.action = na.omit )
}

F_ROUND_ANY <- function(x, accuracy, f=round){
  return( f ( x / accuracy ) * accuracy)
}

# Our custom cluster function
# x = subseted Dataframe (lat, long), you need to do beforehand the logic what data you want
##############################
F_MAP_CLUSTER <- function( x ){
  # I do not really understand how else we should do this, because it is not a real "Cluster" Search
  # Creating our clustering without deep mathematics, high k-means then remove one cluster frame and only keep up to 3 when they to overlap
  x <- x %>% na.omit()
  # kmeans for simple automatic cluster search, 1/4 seems to work good with this data
  # 1/4 for beekeeper distribution
  c <- kmeans(x, ( nrow(x)/4) )
  # Extract Data from kmeans
  CENTERS <- as.data.frame( c$centers )
  CENTERS$cluster <- seq.int( nrow(CENTERS) )  
  CENTERS$n <- c$size
  CENTERS$w <- c$withinss
  CENTERS <- subset( CENTERS, n > 1 ) # remove clusters with 1 point inside, here is the actuall coords from the source better than the cluster center
  CENTERS <- subset( CENTERS, n > 3 | (n < 3 & w == 0) ) # withinss is a measure for disp. inside cluster, by small cluster sizes we want to remove them but if they stacked togheter we keep them
  # Prep. Cache
  x$cluster <- factor( c$cluster )
  x$n <- 1
  x$w <- 0
  # Compare Cache wih our Cluster to only have a cache with points which are not inside the selected cluster
  x <- x[!x$cluster %in% CENTERS$cluster,]
  x <- rbind(CENTERS, x)
  # Arrange DF by n, because if overlapping happens the darkest point (most n) will be at top (example vienna)
  x <- x %>%
    arrange(n)
  return(x)
}
