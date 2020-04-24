##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

# List of Factors we want in our Plot
treatmentList = list(
  c("T_drone_", "T_drone_total", "Drone brood removal"),
  c("T_hyperthermia_", "T_hyperthermia_total", "Hyperthermia"),
  c("T_biotechnical_", "T_biotechnical_total", "Other biotechnical method"),
  c("T_formic_short_", "T_formic_short_total", "Formic acid - short term"),
  c("T_formic_long_", "T_formic_long_total", "Formic acid - long term"),
  c("T_lactic_", "T_lactic_total", "Lactic acid"),
  #c("T_oxalic_trickle_pure_", "T_oxalic_trickle_pure_total", "Oxalic acid - trickling pure"),
  c("T_oxalic_vapo_", "T_oxalic_vapo_total", "Oxalic acid - sublimation"),
  #c("T_oxalic_trickle_mix_", "T_oxalic_trickle_mix_total", "Oxalic acid mixture"),
  c("T_oxalic_trickle_", "T_oxalic_trickle_total", "Oxalic acid - trickling"),
  c("T_thymol_", "T_thymol_total", "Thymol"),
  #c("T_synthetic_apistan_", "T_apistan_total", "Tau-fluvalinat"),
  #c("T_synthetic_flumethrin_", "T_flumethrin_total", "Flumethrin"),
  #c("T_synthetic_amitraz_strips_", "T_amitraz_strips_total", "Amitraz - Strips"),
  #c("T_synthetic_amitraz_vapo_", "T_amitraz_vapo_total", "Amitraz - Vaporize"),
  #c("T_synthetic_coumaphos_p_", "T_coumaphos_p_total", "Coumaphos - Perizin"),
  #c("T_synthetic_coumaphos_c_", "T_coumaphos_c_total", "Coumaphos - Checkmite+"),
  #c("T_synthetic_synother_", "T_chemical_total", "Other Synthetic"),
  c("T_synthetic_", "T_synthetic_total", "Synthetic methods"),
  c("T_other_", "T_other_total", "Another method")
)

# Second List were Mix is not combined with trickle
treatmentListwMix = list(
  c("T_drone_", "T_drone_total", "Drone brood removal"),
  c("T_hyperthermia_", "T_hyperthermia_total", "Hyperthermia"),
  c("T_biotechnical_", "T_biotechnical_total", "Other biotechnical method"),
  c("T_formic_short_", "T_formic_short_total", "Formic acid - short term"),
  c("T_formic_long_", "T_formic_long_total", "Formic acid - long term"),
  c("T_lactic_", "T_lactic_total", "Lactic acid"),
  #c("T_oxalic_trickle_pure_", "T_oxalic_trickle_pure_total", "Oxalic acid - trickling pure"),
  c("T_oxalic_vapo_", "T_oxalic_vapo_total", "Oxalic acid - sublimation"),
  c("T_oxalic_trickle_mix_", "T_oxalic_trickle_mix_total", "Oxalic acid mixture"),
  c("T_oxalic_trickle_", "T_oxalic_trickle_total", "Oxalic acid - trickling"),
  c("T_thymol_", "T_thymol_total", "Thymol"),
  #c("T_synthetic_apistan_", "T_apistan_total", "Tau-fluvalinat"),
  #c("T_synthetic_flumethrin_", "T_flumethrin_total", "Flumethrin"),
  #c("T_synthetic_amitraz_strips_", "T_amitraz_strips_total", "Amitraz - Strips"),
  #c("T_synthetic_amitraz_vapo_", "T_amitraz_vapo_total", "Amitraz - Vaporize"),
  #c("T_synthetic_coumaphos_p_", "T_coumaphos_p_total", "Coumaphos - Perizin"),
  #c("T_synthetic_coumaphos_c_", "T_coumaphos_c_total", "Coumaphos - Checkmite+"),
  #c("T_synthetic_synother_", "T_chemical_total", "Other Synthetic"),
  c("T_synthetic_", "T_synthetic_total", "Synthetic methods"),
  c("T_other_", "T_other_total", "Another method")
)

fulltreatmentList = list(
  c("T_drone_", "T_drone_total", "Drohnenbrutentnahme"),
  c("T_hyperthermia_", "T_hyperthermia_total", "Hyperthermie - Anwendung"),
  c("T_biotechnical_", "T_biotechnical_total", "Andere biotechnische Methode"),
  c("T_formic_short_", "T_formic_short_total", "Ameisensäure - Kurzzeit"),
  c("T_formic_long_", "T_formic_long_total", "Ameisensäure - Langzeit"),
  c("T_lactic_", "T_lactic_total", "Milchsäure"),
  c("T_oxalic_trickle_pure_", "T_oxalic_trickle_pure_total", "Oxalsäure - Träufeln o. Sprühen"),
  c("T_oxalic_vapo_", "T_oxalic_vapo_total", "Oxalsäure - Verdampfen"),
  c("T_oxalic_trickle_mix_", "T_oxalic_trickle_mix_total", "Oxalsäure - Mischung(*)"),
  #c("T_oxalic_trickle_all_", "T_oxalic_trickle_all_total", "Oxalic acid - trickling all methods"),
  c("T_thymol_", "T_thymol_total", "Thymol"),
  c("T_synthetic_apistan_", "T_apistan_total", "Tau-fluvalinat"),
  c("T_synthetic_flumethrin_", "T_flumethrin_total", "Flumethrin"),
  c("T_synthetic_amitraz_strips_", "T_amitraz_strips_total", "Amitraz - Streifen"),
  c("T_synthetic_amitraz_vapo_", "T_amitraz_vapo_total", "Amitraz - Verdampfen"),
  c("T_synthetic_coumaphos_p_", "T_coumaphos_p_total", "Coumaphos - Perizin"),
  c("T_synthetic_coumaphos_c_", "T_coumaphos_c_total", "Coumaphos - Checkmite+"),
  c("T_synthetic_synother_", "T_chemical_total", "Anderes chem. Produkt"),
  #c("T_synthetic_", "T_synthetic_total", "Synthetic methods"),
  c("T_other_", "T_other_total", "Andere Methode")
)

#### SPRING Treatment Values ####
# Dummy List
D.CACHE <- list()
# Loop through our Treatment Types
for(i in treatmentList){
  # Get Columns which are starting with List value
  treatmentexp <- paste("(", i[1], ")\\S*0[1-2]", sep = "")
  x <- grepl(treatmentexp, colnames(D.RAW), fixed = FALSE, perl = TRUE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE[[paste(i[2], "_spring", sep = "")]] <- rowSums(D.RAW[, x], na.rm = TRUE)
  # create a yes no list too
  xn <- paste( i[2], "yn_spring", sep = "")
  D.CACHE[[xn]] <- ifelse((rowSums(D.RAW[, x], na.rm = TRUE)) > 0, 1, 0)
}
# Convert List to Dataframe
D.CACHE <- data.frame(D.CACHE)
D.RAW <- cbind(D.RAW, D.CACHE)

#### SUMMER Treatment Values ####
# Dummy List
D.CACHE <- list()
# Loop through our Treatment Types
for(i in treatmentList){
  # Get Columns which are starting with List value
  treatmentexp <- paste("(", i[1], ")\\S*0[3-7]", sep = "")
  x <- grepl(treatmentexp, colnames(D.RAW), fixed = FALSE, perl = TRUE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE[[paste(i[2], "_summer", sep = "")]] <- rowSums(D.RAW[, x], na.rm = TRUE)
  # create a yes no list too
  xn <- paste( i[2], "yn_summer", sep = "")
  D.CACHE[[xn]] <- ifelse((rowSums(D.RAW[, x], na.rm = TRUE)) > 0, 1, 0)
}
# Convert List to Dataframe
D.CACHE <- data.frame(D.CACHE)
D.RAW <- cbind(D.RAW, D.CACHE)

#### WINTER Treatment Values ####
# Dummy List
D.CACHE <- list()
# Loop through our Treatment Types
for(i in treatmentList){
  # Get Columns which are starting with List value
  treatmentexp <- paste("(", i[1], ")\\S*0[8-9]|(", i[1], ")\\S*1[0]", sep = "")
  x <- grepl(treatmentexp, colnames(D.RAW), fixed = FALSE, perl = TRUE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE[[paste(i[2], "_winter", sep = "")]] <- rowSums(D.RAW[, x], na.rm = TRUE)
  # create a yes no list too
  xn <- paste( i[2], "yn_winter", sep = "")
  D.CACHE[[xn]] <- ifelse((rowSums(D.RAW[, x], na.rm = TRUE)) > 0, 1, 0)
}
# Convert List to Dataframe
D.CACHE <- data.frame(D.CACHE)
D.RAW <- cbind(D.RAW, D.CACHE)

#### TOTAL Treatment Values ####
# Dummy List
D.CACHE <- list()
# Loop through our Treatment Types
for(i in treatmentList){
  # Get Columns which are starting with List value
  # double blackslash otherwise R wont escape the backslash
  treatmentexp <- paste("(", i[1], ")\\S*0[1-9]|(", i[1], ")\\S*1[0]", sep = "")
  x <- grepl(treatmentexp, colnames(D.RAW), fixed = FALSE)
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  D.CACHE[[i[2]]] <- rowSums(D.RAW[, x], na.rm = TRUE)
  # create a yes (1) no (2) list too
  xn <- paste( i[2], "_yn", sep = "")
  D.CACHE[[xn]] <- ifelse((rowSums(D.RAW[, x], na.rm = TRUE)) > 0, 1, 0)
}

# sum rows for total different methods and seasons
# sum rows by yn column, that way we get amount of different treatments used
x <- grep("(yn_)", colnames(D.RAW), fixed = FALSE)
D.RAW$T_amount_total <- rowSums(D.RAW[, x], na.rm = TRUE)

# Convert List to Dataframe
D.CACHE <- data.frame(D.CACHE)
# sum rows by yn column, that way we get amount of different treatments used
x <- grep("_yn", colnames(D.CACHE), fixed = TRUE)
D.CACHE$T_amount <- rowSums(D.CACHE[, x], na.rm = TRUE)
D.RAW <- cbind(D.RAW, D.CACHE)

