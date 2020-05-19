### Survey Bee Hive Losses ###
# (c) 2019-2020 Hannes Oberreiter #

# ---- Custom Functions CODE ----

# GLM Single, without Factor
# x = Dataframe
F_GLM_SINGLE <- function( x )
{
  GLM.FULL <- glm( 
    cbind( hives_lost_e, hives_spring_e ) ~ 1, 
    family = quasibinomial( link = "logit" ), 
    data = x, na.action = na.omit )
  SUMMARY.FULL <- summary( GLM.FULL )
  print( SUMMARY.FULL ) 
  GLM.ODDS <- F_NUMBER_FORMAT( GLM.FULL$fitted.values[1] * 100 )
  GLM.LOW <- F_NUMBER_FORMAT( inv.logit( coef( GLM.FULL) - qt( 0.975, df = GLM.FULL$df.residual ) * SUMMARY.FULL$coefficients[, 2] ) * 100 )
  GLM.MAX <- F_NUMBER_FORMAT( inv.logit( coef( GLM.FULL) + qt( 0.975, df = GLM.FULL$df.residual ) * SUMMARY.FULL$coefficients[, 2] ) * 100 )
  return( CACHE.BIND <- cbind( lowerlim = GLM.LOW, middle = GLM.ODDS, upperlim = GLM.MAX ) )
}

# GLM, with Factor
# x = Dataframe, f = factor as string, xf = factor from dataframe, chi = TRUE when you want to return column with stars if chisqr is significant, na_omit = standard TRUE, if you want NAs reported set to false
F_GLM_FACTOR <- function( x, f, xf, chi = FALSE, na_omit = TRUE)
{
  # get column via string
  x$ff <- get( f, pos = x ) 
  # create string for NAs otherwise they will be removed
  if(!na_omit){
    x$ff[is.na(x$ff)] <- 'No Answer'
    xf[is.na(xf)] <- 'No Answer'
  } else {
    x <- x[!is.na(x$ff),]
  }
  x$ff <- as.factor(x$ff)

  GLM.FULL <- glm( 
    cbind( hives_lost_e, hives_spring_e ) ~ ff ,
    family = quasibinomial( link = "logit" ), 
    data = x, na.action = na.omit )
  SUMMARY.FULL <- summary( GLM.FULL )
  print( SUMMARY.FULL ) 
  # F-Test
  ANOVA.FULL <- anova( GLM.FULL, test = "F" )
  write.csv( ANOVA.FULL, file = paste("./ANOVA/", paste(f, "_ANOVA.csv", sep = "" ), sep = "" ) )
  # Chi-Sqr Test
  ANOVA.CHISQ <- anova( GLM.FULL, test = "Chisq" )
  #TODO if p value is significant create pairwise comparison?
  chistar <- ""
  if(chi){
    if(ANOVA.CHISQ[[5]][2] < 0.05){
      chistar <- TRUE
    } else {
      chistar <- FALSE
    }
  }

  write.csv( ANOVA.CHISQ, file = paste("./ANOVA/", paste(f, "_CHISQ.csv", sep = "" ), sep = "" ) )
  # Print Output
  print( paste( "ANOVA TEST --> ", f ))
  print( ANOVA.FULL ) 
  
  # Calculate odds via model for factors  
  DFRESIDUAL.FULL <- SUMMARY.FULL$df.residual
  VALUES.FULL <- predict( GLM.FULL, data.frame( ff = levels( as.factor(xf) ) ), type = "link", se.fit = T )
  
  # Approximate 95% CIs for the log odds & upper and lower limit (NOT WITH REFERENCE REGION, Dunno)
  CACHE.ODDS <- VALUES.FULL$fit
  CACHE.LOWERLIM <- VALUES.FULL$fit - qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  CACHE.UPPERLIM <- VALUES.FULL$fit + qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  # Add Prob. to our plot matrix
  CACHE.ODDS = F_NUMBER_FORMAT( inv.logit( CACHE.ODDS ) * 100 )
  CACHE.LOWERLIM = F_NUMBER_FORMAT( inv.logit( CACHE.LOWERLIM ) * 100 )
  CACHE.UPPERLIM = F_NUMBER_FORMAT( inv.logit( CACHE.UPPERLIM ) * 100 )
  
  if(chi)   return( CACHE.BIND <- cbind( lowerlim = CACHE.LOWERLIM, middle = CACHE.ODDS, upperlim = CACHE.UPPERLIM, chistar = chistar ) )
  return( CACHE.BIND <- cbind( lowerlim = CACHE.LOWERLIM, middle = CACHE.ODDS, upperlim = CACHE.UPPERLIM ) )
}

# Number Format, round to 2 after decimal
# x = number
F_NUMBER_FORMAT <- function(x)
{
  x <- as.numeric( format( round( x, 1 ), nsmall = 2))
  return(x)
}

# Extract our N into a DF
# x = Dataframe, f = level, c = Name for Factor
F_EXTRACT_N <- function( x_df, column, type_name, omit_na = TRUE )
  {
  # get our factor position inside the dataframe
  x_df$ff = get( column, pos = x_df ) 
  F.CACHE <- x_df %>% 
    group_by( ff ) %>% 
    summarize(
      c = type_name,
      n = n(),
      hives_winter = sum(hives_winter),
      lost_a = sum(lost_a, na.rm = FALSE),
      lost_b = sum(lost_b, na.rm = FALSE),
      lost_c = sum(lost_c, na.rm = FALSE),
      hive_lost_rate = as.numeric( format( round(
                ( sum( hives_lost_e ) / sum( hives_winter ) * 100 ), 1), nsmall = 2))
    )
  print( F.CACHE )
  if(omit_na){
    F.CACHE <- F.CACHE %>% na.omit()
  }
  return( F.CACHE )
}

# Combination Calcuation
# x = Dataframe, d = col numbers, itn = iternations, CacheList = "nice" names of cols, 
# ColComb1 = our single intereger array of all treatments collumns,
# negative = standard 1, then it selecteds only participants which did use the combination, set to 0 to select the people which did not use this combination
F_COMBINATION <- function( x, d, itn, CacheList, ColComb1, negative = 1 ){
  # dummy list will later be converted to dataframe
  D.COMB <-list()
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
    cname = ""
    sname = ""
    shortname = ""
    # loop for multiple combination, single treatment method is only single run
    for( n in 1:itn ){
      # remove selected treatment(s) from the list of treatment columns
      d1 <- d1 [ d1 != i[ n ] ]
      # subsetting our dataframe, were only our selected treatments are given
      CACHE.COMB <- CACHE.COMB[ CACHE.COMB[, i[n]] == negative , ]
      coln <- paste( coln, colnames(CACHE.COMB[i[n]]), sep = " ")
      # Names for Export
      cname[n] = as.character( CacheList$X3[ CacheList$V.ColComb1 == i[n] ])
      sname[n] = as.character( CacheList$X2[ CacheList$V.ColComb1 == i[n] ])

      shortname <- ifelse(n == 1,
                          paste(CacheList$X4[ CacheList$V.ColComb1 == i[n] ]),
                          paste(shortname, CacheList$X4[ CacheList$V.ColComb1 == i[n]], sep = "&" )
                          )
    }
    
    # if no data is available we skip it
    if( nrow( CACHE.COMB ) == 0 ) next

    if(negative == 1){
      # count numbers without given treatment, if it is bigger than 0 it means there are other treatments in combination
      # drop = FALSE tocreate a one-column dataframe and no vector if there is only one column left
      #print(d1)
      CACHE.COMB$comb_count <- rowSums(CACHE.COMB[ , d1, drop = FALSE ], na.rm = TRUE)
      #print(CACHE.COMB$comb_count)
      # only get the count 0 ones, because then we are sure there is no combination
      CACHE.COMB <- CACHE.COMB[CACHE.COMB[, "comb_count"] == 0, ]
    }

    # get loss probability for combination
    # create dummy columns, to insert the values from GLM
    CACHE.BIND <- c(NA,NA,NA)
    # if less than 15 rows we skip it
    #if( nrow( CACHE.COMB ) < 15) next
    if( nrow( CACHE.COMB ) == 0) next
    if( nrow( CACHE.COMB ) > 9) CACHE.BIND <- F_GLM_SINGLE( CACHE.COMB )

    # Get Costs of Varroa Treatment per Hive
    cost <- summary(CACHE.COMB$costs, na.rm = TRUE)

        # create cost dataframe without na's
    cost.df <- CACHE.COMB$costs[!(is.na(CACHE.COMB$costs))]
    cost.df <- tibble(cost.df)
    names(cost.df) <- "a"
    cost.df$b <- 1
    # send to bootstrap 

        cost.boot <- F_BOOTSTRAP(cost.df, shortname, 1)
    # Standard Derivation from mean with 95% square root n, using qt because t-distribution
    #sd.cost <- sd(CACHE.COMB$costs, na.rm = TRUE)
    #se.cost = sd.cost / sqrt(cost_count)
    #Q <- qt(1 - (0.05 / 2), df = cost_count - 1)
    #lower.ci.cost = cost[[4]] - Q * se.cost
    #upper.ci.cost = cost[[4]] + Q * se.cost
    # If lower CI is less than 0 euro then we set it to 0 euro
    #lower.ci.cost <- ifelse(lower.ci.cost < 0, 0, lower.ci.cost)
    
    # Create Histogramm of costs
    ghisto_DF <- CACHE.COMB[!(is.na(CACHE.COMB$costs)), ]
    ghisto_Plot <- ggplot(ghisto_DF, aes(x = costs)) + geom_histogram()
    histo_save <- paste("./histo/Hist_", shortname ,".pdf", sep = "")
    ggsave(histo_save, ghisto_Plot, width = 5, height = 5, units = "in")

    
    D.COMB[[counter]] <- tibble(
      t = itn, 
      negative = negative,
      
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
      short = shortname,
      
      n = nrow( CACHE.COMB ), 
      lowerlim = CACHE.BIND[1], 
      middle = CACHE.BIND[2], 
      upperlim = CACHE.BIND[3],
      
      c_min = cost[[1]],
      c_1q = cost[[2]],
      c_median = cost[[3]],
      c_mean = cost.boot$mean,
      c_3q = cost[[5]],
      c_max = cost[[6]],
      c_n = cost.boot$n,
      c_ci_upper = cost.boot$upper.ci,
      c_ci_lower = cost.boot$lower.ci
      
    )
    
  }
  # Return full DF
  return (bind_rows(D.COMB))
}

# Our custom cluster function
# x = subseted Dataframe (lat, long), you need to do beforehand the logic what data you want
# number_clusters = divides the n with the given number, it tells us how many clusters we want to find
F_MAP_CLUSTER <- function( x, number_clusters = 2){
  # I do not really understand how else we should do this, because it is not a real "Cluster" Search
  # Creating our clustering without deep mathematics, high k-means then remove one cluster frame and only keep up to 3 when they to overlap
  x <- x %>% na.omit()
  # kmeans for simple automatic cluster search, 1/4 seems to work good with this data
  # 1/4 for beekeeper distribution
  c <- kmeans(x, ( nrow(x)/number_clusters) )
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

# Extracts a legend from a ggplot
F_GG_LEGEND <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Bootstrap sample method to calculate mean of full sample and corresonding CI, with not normally distributed data
# atm. only used in total losses for proliferation rate
# we use it because the data is not normally distributed and we cannot fit GLM
# df = dataframe contain columns a (difference), b (sum end), f (factor), percent (if percent then you can leave empty otherwise use 1)
F_BOOTSTRAP <- function(df, fact, percent = 100){
  # not worth to calculate anything if sample is below 5
  if(nrow(df) < 5){
    print("Not enough samples for bootstrapping")
    x <- data.frame(
      "state" = NA, 
      "n" = NA,
      "mean" = NA,
      "lower.ci" = NA,
      "upper.ci" = NA
    )
    return( x )
  }

  
  # Simple Mean Function, will calculate total sample mean
  mean.fun <- function(d, i) 
  {    
    # mean of difference (eg. prolifer. hives_production)
    mean.a <- mean(d$a[i])
    # mean of end value (eg. proflifer. hives_winter)
    mean.b <- mean(d$b[i])
    # divde means to get full sample profileration rate
    c <- mean.a / mean.b
    return (c)
  }
  # bootstrap 999 times
  sample.boot <- boot(df, mean.fun, R = 2000)
  # bias corrected sample mean = mean of bootstrap samples - original sample is bias correcture value
  # https://www.statmethods.net/advstats/bootstrapping.html
  # https://stats.stackexchange.com/questions/372589/explanation-of-confidence-interval-from-r-function-boot-ci
  # https://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
  s.mean <- sample.boot$t0 - (mean(sample.boot$t[,1]) - sample.boot$t0)
  # calculating CI95 with the bootstrap samples
  # using basic method here as it seems more accurate than percentile bootstrap with lower n's
  # https://stats.stackexchange.com/questions/37918/why-is-the-error-estimated-adjustment-a-is-na-generated-from-r-boot-package
  sample.boot.si <- boot.ci(sample.boot, type = "basic", conf = 0.95)
  # format to percentage
  s.n <- nrow(df)
  s.mean <- F_NUMBER_FORMAT(s.mean * percent)
  s.lower <- F_NUMBER_FORMAT(sample.boot.si$basic[,4] * percent)
  s.upper <- F_NUMBER_FORMAT(sample.boot.si$basic[,5] * percent)
  
  # Save Plot to check later if corrrect
  s.plot.name <- paste("./bootstrap/Boot_", fact ,".pdf", sep = "")
  pdf(s.plot.name)
  plot(sample.boot)
  dev.off()
  
  # Create a Dataframe which we return
  x <- data.frame(
    "state" = fact, 
    "n" = s.n,
    "mean" = s.mean,
    "lower.ci" = s.lower,
    "upper.ci" = s.upper
  )
  return( x )
}

# Function which generates a dataframe to draw significant results onto plot
# D.INPUT = our initial dataframe (see return from F_GLM_FACTOR)
# V.START and V.END the two groups to connect
# V.GROUP if multiple plots are arranged with facet_grid we need here to define column which are used for grouping the grid
# V.LABEL here we can define what we want to see above the connected significant line
F_CHISTAR_DF <- function(D.INPUT, V.START = "Ja", V.END = "Nein", V.GROUP = FALSE, V.LABEL = "*"){
  # generate chistar brackets temporary dataframe
  D.TEMP <- D.INPUT[D.INPUT$chistar == 1,]

  if(nrow(D.TEMP) == 0){
    print("No signifikant results inside given DF")
    return(D.ANNOTATION <- data.frame())
  }
  
  D.ANNOTATION <- 
    data.frame(
      start  = D.TEMP$ff[D.TEMP$ff == V.START], 
      end    = D.TEMP$ff[D.TEMP$ff == V.END], 
      y      = ifelse(
        (D.TEMP$upperlim[D.TEMP$ff == V.START] > D.TEMP$upperlim[D.TEMP$ff == V.END]), 
        D.TEMP$upperlim[D.TEMP$ff == V.START],
        D.TEMP$upperlim[D.TEMP$ff == V.END]) + 2, 
      label  = V.LABEL
    )
  
  if(V.GROUP != FALSE){
    D.ANNOTATION <- cbind(D.ANNOTATION, D.TEMP[D.TEMP$ff == V.START, c(V.GROUP)])
    names(D.ANNOTATION)[5] <- V.GROUP
  }
  
  return (D.ANNOTATION)
}


# Simple function to generate a single plot
# we use this to make easier changes on all plots if necessary
F_SINGLE_PLOT <- function(
  df, significant = data_frame(empty=numeric()), 
  barfill="white", xangle = 0, xhjust = 0.5,
  ptitle = FALSE
  ){
  p <- ggplot(data = df) +
    aes( x = ff, y = middle) +
    geom_crossbar(aes( ymin = lowerlim, ymax = upperlim, fill = I(barfill) )) +
    geom_point(size = 3) + 
    geom_text( aes( x = ff, y = 0.5, label = paste("n = ", n )), angle = 0, vjust = 0, color = "black", size = 2.5 ) +
    xlab("") + ylab("Verlustrate [%]") + 
    theme_classic() + 
    theme(
      panel.spacing = unit( 1, "lines" ),
      strip.placement = "outside",
      plot.title = element_text(), 
      axis.title.x = element_text(colour = "black" ), 
      axis.text.x = element_text(angle = xangle, hjust = xhjust, size = 9, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.line = element_line( linetype = "solid" ),
      panel.grid.major.y = element_line( colour = "grey" ),
      panel.grid.minor.y = element_line( colour = "grey" )
    ) +
    scale_x_discrete(
    ) +
    scale_y_continuous(
      limit = c(0, max(df$upperlim)+5),
      expand = c( 0 , 0 ),
      breaks = seq( 0, 45, 5 )
    )
  
  if(nrow(significant)>0){
    p <- p + geom_signif(data=significant, aes(xmin=start, xmax=end, annotations=label, y_position=y), textsize = 8, manual=TRUE)
  }
  if(ptitle != FALSE){
    p <- p + ggtitle(ptitle)
  }
  
  return(p)
}

# Histo Blar Plots
F_HISTO_PLOT <- function(df, xtext, ytext, ttext){
  p <- ggplot( data = df ) +
    aes( x = val, y = n) + 
    geom_bar( colour = "black", alpha = 1, fill = "black", show.legend = FALSE, stat = "identity", linetype = "solid") + 
    geom_text( aes( label = paste(np, "%", sep = "" )), angle = 55, vjust = -0.5, hjust = 0, color = "black", size = 3 ) +
    xlab(xtext) + ylab(ytext) + 
    ggtitle(ttext) +
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0), 
      axis.title.x = element_text(colour = "black" ), 
      axis.text.x = element_text(angle = -55, hjust = 0, size = 8, face = "bold"),
      axis.line = element_line( linetype = "solid" )
    ) +
    scale_x_discrete(
    ) +
    scale_y_continuous(
      expand = c( 0 , 0 ),
      breaks = seq( 0, max(df$n)*1.1, 100 ),
      limits = c( 0, max(df$n)*1.1 )
    )
  
  return(p)
}



