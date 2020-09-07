### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# --- Treatment Export ----

# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Import ----
source( "Partials_Header.r" )
source( "Partials_Header_Treatment.r" )
source( "Partials_Functions.r" )

# ----- START CODE -----
# RAW into our working dataframe
# Only Part. which did answer any treatment
D.FULL <- D.RAW[D.RAW$T_amount>0,]

# Generate full Treatment List cols
D.CACHE <- list()
for(i in fulltreatmentList){
  # Get Columns which are starting with List value
  # double blackslash otherwise R wont escape the backslash
  treatmentexp12 <- paste("(", i[1], ")\\S*0[1-9]|(", i[1], ")\\S*1[0-2]", sep = "")
  x12 <- grepl(treatmentexp12, colnames(D.FULL), fixed = FALSE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE[[paste(i[2], "12a", sep = "")]] <- rowSums(D.FULL[, x12], na.rm = TRUE)
}
D.FULL <- cbind(D.FULL, D.CACHE)

# Extract numbers, we use 12 months here, see above
D.TREAT <- tibble()
for(i in fulltreatmentList){
  s <- paste(i[2], "12a", sep="")
  yes <- D.FULL %>% summarize(sum = sum(eval(parse(text=paste(s)))>0))
  D.TREAT <- rbind(D.TREAT, c(name = i[3], yes = yes, no = nrow(D.FULL)-yes))
}
rm(i, s, yes)

# add special cases

x <- apply(D.FULL, 1, function(d){
  r <- ifelse(d["T_formic_long_total12"] > 0 || d["T_formic_short_total12"] > 0, TRUE, FALSE)
  return(r)
} )
D.TREAT <- rbind(D.TREAT, c(name = "Ameisensäure (Kurz oder Lang)", yes = sum(x), no = nrow(D.FULL)-sum(x)))

t <- D.FULL[x,]
t <- t[t$T_oxalic_trickle_total12 > 0 | t$T_oxalic_vapo_total12 > 0,]
D.TREAT <- rbind(D.TREAT, c(name = "Ameisensäure (Kurz oder Lang) + Oxalsäure", yes = nrow(t), no = nrow(D.FULL)-nrow(t)))
D.TREAT$yes.sum <- as.numeric(D.TREAT$yes.sum)
D.TREAT$no.sum <- as.numeric(D.TREAT$no.sum)

D.TREAT$yes.per <- round(D.TREAT$yes.sum / (D.TREAT$yes.sum + D.TREAT$no.sum) * 100,1)
D.TREAT$no.per <- 100 - D.TREAT$yes.per
D.TREAT <- D.TREAT[, c(1, 2, 4, 3, 5)]

rm(t, x)
knitr::kable(D.TREAT, format="latex", booktabs=TRUE)
write_excel_csv2( D.TREAT, path = paste("./", "Treatment_List.csv", sep = "" ) )