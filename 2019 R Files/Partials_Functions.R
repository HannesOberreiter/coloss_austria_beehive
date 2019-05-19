##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

# Our Custom Functions

#### GLM Single #### 
# x = Dataframe
##############################
F_GLM_SINGLE <- function( x )
{
  GLM.FULL <- glm( 
    cbind( hives_lost_e, hives_spring_e ) ~ 1, 
    family = quasibinomial( link = "logit" ), 
    data = x, na.action = na.omit )
  SUMMARY.FULL <- summary( GLM.FULL )
  GLM.ODDS <- GLM.FULL$fitted.values[1] * 100
  GLM.LOW <- inv.logit( coef( GLM.FULL) - qt( 0.975, df = GLM.FULL$df.residual ) * SUMMARY.FULL$coefficients[, 2] ) * 100
  GLM.MAX <- inv.logit( coef( GLM.FULL) + qt( 0.975, df = GLM.FULL$df.residual ) * SUMMARY.FULL$coefficients[, 2] ) * 100
  return( CACHE.BIND <- cbind( lowerlim = GLM.LOW, middle = GLM.ODDS, upperlim = GLM.MAX ) )
}


#### GLM with Factors #### 
# x = Dataframe, f = factor as string, xf is factor from dataframe
##############################
F_GLM_FACTOR <- function( x, f, xf ){
  x$ff <- get( f, pos = x ) 
  #print(x$ff)
  GLM.FULL <- glm( 
    cbind( hives_lost_e, hives_spring_e ) ~ as.factor( ff ) , # F*** it, we need to use example "Bundesland" here
    family = quasibinomial( link = "logit" ), 
    data = x, na.action = na.omit )
  SUMMARY.FULL <- summary( GLM.FULL )
  print( SUMMARY.FULL ) 
  # Check which DF we should use
  ANOVA.FULL <- anova( GLM.FULL, test = "F" )
  print( paste( "ANOVA TEST --> ", f ))
  print( ANOVA.FULL ) 
  # Save Summary to ANOVA Folder
  write.csv( ANOVA.FULL, file = paste("./ANOVA/", paste(f, "_ANOVA.csv", sep = "" ), sep = "" ) )
  
  ANOVA.CHISQ <- anova( GLM.FULL, test = "Chisq" )
  write.csv( ANOVA.CHISQ, file = paste("./ANOVA/", paste(f, "_CHISQ.csv", sep = "" ), sep = "" ) )
  
  
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

#### Extract our N into a DF #### 
# x = Dataframe, f = level, c = Name for Factor
##############################
F_EXTRACT_N <- function( x, f, c ){
  x$ff = get( f, pos = x ) 
  F.CACHE <- x %>% 
    group_by( ff ) %>% 
    summarize(
      c = c,
      n = n(),
      hives_winter = sum(hives_winter),
      lost_a = sum(lost_a, na.rm = FALSE),
      lost_b = sum(lost_b, na.rm = FALSE),
      lost_c = sum(lost_c, na.rm = FALSE),
      hives_lost_rate = sum( hives_lost_e ) / sum( hives_winter ) * 100
    )
  print( F.CACHE )
  F.CACHE <- F.CACHE %>% na.omit()
  return( F.CACHE )
}

#### Combination Calcuation #### 
# x = Dataframe, d = col numbers, itn = iternations, CacheList = "nice" names of cols, ColComb1 = our single intereger array of all treatments collumns
##############################
F_COMBINATION <- function( x, d, itn, CacheList, ColComb1 ){
  # our dummy dataframe, which will be returned
  D.COMB <- 
    setNames( 
      data.frame( matrix( ncol = 25, nrow = 0)), 
      c( "t", "s1", "c1", "s2", "c2", "s3", "c3", "s4", "c4", "s5", "c5", "s6", "c6", "colnames", "n", "lowerlim", "middle", "upperlim", "c_min", "c_1q", "c_median", "c_mean", "c_3q", "c_max", "c_n")
    )
  counter <- 1
  countermax <- length(d)
  # loop through column numbers
  for(i in d){
    # Progress Bar
    progress(counter, max.value = countermax, progress.bar = FALSE)
    counter <- counter + 1
    # save col numbers to a second variable, we use later
    d1 <- ColComb1
    # save our df to our cache variable
    CACHE.COMB <- x
    # temporary colnames, to check later if values are ordered to correct treatments in df
    coln <- ""
    # reset our name fields
    cname = list()
    sname = list()
    # loop for multiple combination, single treatment method is only single run
    for( n in 1:itn ){
      # remove selected treatment(s) from the list of treatment columns
      d1 <- d1 [ d1 != i[ n ] ]
      # subsetting our dataframe, were only our selected treatments are given
      CACHE.COMB <- CACHE.COMB[ CACHE.COMB[, i[n]] == 1 , ]
      coln <- paste( coln, colnames(CACHE.COMB[i[n]]), sep = " ")
      # Names for Export
      cname[n] = as.character( CacheList$X3[ CacheList$ColComb1 == i[n] ])
      sname[n] = as.character( CacheList$X2[ CacheList$ColComb1 == i[n] ])
    }
    
    # if no data is available we skip it
    if( nrow( CACHE.COMB ) == 0 ) next
      
    # count numbers without given treatment, if it is bigger than 0 it means there are other treatments in combination
    # drop = FALSE tocreate a one-column dataframe and no vector if there is only one column left
    CACHE.COMB$comb_count <- rowSums(CACHE.COMB[ , d1, drop = FALSE ], na.rm = TRUE)
    # only get the count 0 ones, because then we are sure there is no combination
    CACHE.COMB <- CACHE.COMB[CACHE.COMB[, "comb_count"] == 0, ]
    # get loss probability for combination
    CACHE.BIND <- c(NA,NA,NA)
    
    # if less than 10 rows we skip it
    if( nrow( CACHE.COMB ) < 10) next
    if( nrow( CACHE.COMB ) > 9) CACHE.BIND <- F_GLM_SINGLE( CACHE.COMB )
    
    # Get Costs of Varroa Treatment per Hive
    cost <- summary(CACHE.COMB$costs, na.rm = TRUE)
    # Count rows with values
    cost_count <- nrow ( CACHE.COMB[!(is.na(CACHE.COMB$costs)), ] ) 
    
    # Add row to Dummy DF
    D.COMB <- D.COMB %>% add_row(
      t = itn, 
      
      s1 = sname[1],
      c1 = cname[1],
      s2 = sname[2],
      c2 = cname[2],
      s3 = sname[3],
      c3 = cname[3],
      s4 = sname[4],
      c4 = cname[4],
      s5 = sname[5],
      c5 = cname[5],
      s6 = sname[6],
      c6 = cname[6],
      
      colnames = coln,
      n = nrow( CACHE.COMB ), 
      lowerlim = CACHE.BIND[1], 
      middle = CACHE.BIND[2], 
      upperlim = CACHE.BIND[3],
      
      c_min = cost[[1]],
      c_1q = cost[[2]],
      c_median = cost[[3]],
      c_mean = cost[[4]],
      c_3q = cost[[5]],
      c_max = cost[[6]],
      c_n = cost_count
    )
  }
  # Return full DF
  return (D.COMB)
}

#### Our custom cluster function #### 
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

#### Unused Function #### 
##############################
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
