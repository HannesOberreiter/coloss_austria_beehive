##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################


treatmentList = tibble(
  tsingle = c(
    "T_vcount_", 
    "T_drone_",
    "T_hyperthermia_",
    "T_biotechnical_",
    "T_formic_short_",
    "T_formic_long_",
    "T_lactic_",
    #"T_oxalic_trickle_pure_",
    "T_oxalic_vapo_",
    #"T_oxalic_trickle_mix_",
    "T_oxalic_trickle_",
    "T_thymol_",
    "T_synthetic_",
    "T_other_"
  ),
  ttotal = NA,
  tname = c(
    "Varroa Kontrolle",
    "Drohnenbrutentnahme",
    "Hyperthermie",
    "Andere biotechnische\nMethode",
    "Ameisensäure - Kurzzeit",
    "Ameisensäure - Langzeit",
    "Milchsäure",
    #"Oxalsäure - Pure",
    "Oxalsäure - Verdampfen",
    #"Oxalsäure - Mix",
    "Oxalsäure - Träufeln oder\nSprühen inkl. Mischung(*)",
    "Thymol",
    "Chemische Methoden(+)",
    "Andere Methode"
  ),
  tshort = c(
    "V-check",
    "Drohne",
    "Hyp.",
    "Biot.",
    "AS-KZ",
    "AS-LZ",
    "Milchs.",
    #"Ox-pure",
    "Ox-sub",
    #"Ox-mix",
    "Ox-trickle",
    "Thy",
    "chem. Pr.",
    "Andere"
  )
)

treatmentList$ttotal <- paste(treatmentList$tsingle, "total", sep="")

# Second List were Mix is not combined with trickle
treatmentListwMix = list(
  c("T_drone_", "T_drone_total", "Drone brood removal"),
  c("T_hyperthermia_", "T_hyperthermia_total", "Hyperthermie - Anwendung"),
  c("T_biotechnical_", "T_biotechnical_total", "Other biotechnical method"),
  c("T_formic_short_", "T_formic_short_total", "Ameisensäure - Kurzzeit"),
  c("T_formic_long_", "T_formic_long_total", "Ameisensäure - Langzeit"),
  c("T_lactic_", "T_lactic_total", "Milchsäure"),
  #c("T_oxalic_trickle_pure_", "T_oxalic_trickle_pure_total", "Oxalic acid - trickling pure"),
  c("T_oxalic_vapo_", "T_oxalic_vapo_total", "Oxalsäure - Verdampfen"),
  c("T_oxalic_trickle_mix_", "T_oxalic_trickle_mix_total", "Oxalsäure - Mischung(*)"),
  c("T_oxalic_trickle_", "T_oxalic_trickle_total", "Oxalic acid - trickling"),
  c("T_thymol_", "T_thymol_total", "Thymol"),
  #c("T_synthetic_apistan_", "T_apistan_total", "Tau-fluvalinat"),
  #c("T_synthetic_flumethrin_", "T_flumethrin_total", "Flumethrin"),
  #c("T_synthetic_amitraz_strips_", "T_amitraz_strips_total", "Amitraz - Strips"),
  #c("T_synthetic_amitraz_vapo_", "T_amitraz_vapo_total", "Amitraz - Vaporize"),
  #c("T_synthetic_coumaphos_p_", "T_coumaphos_p_total", "Coumaphos - Perizin"),
  #c("T_synthetic_coumaphos_c_", "T_coumaphos_c_total", "Coumaphos - Checkmite+"),
  #c("T_synthetic_synother_", "T_chemical_total", "Other Synthetic"),
  c("T_synthetic_", "T_synthetic_total", "Chemische Methoden(+)"),
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


seasons <- tibble(
  name = c("spring", "summer", "winter"),
  rcol = c("0[1-2]", "0[3-7]", "0[8-9]"),
  short = c("SP", "SU", "WI"),
  desc = c("SPRING", "SUMMER", "WINTER")
)

# Temporary for Combinations
D.RAW$c_short <- NA
D.RAW$c_desc  <- NA

for(i in 1:nrow(seasons)){
  #print(paste0("Season: ", seasons$name[i]))
  for(j in 1:nrow(treatmentList)){
    #print(paste0("Treatment: ", treatmentList$tname[j]))
    treatmentexp <- paste(
      "(", treatmentList$tsingle[j], ")\\S*", seasons$rcol[i], 
      sep = ""
    )
    # Get Columns which are starting with List value
    x       <- grepl(treatmentexp, colnames(D.RAW), fixed = FALSE, perl = TRUE)
    rsums   <- rowSums(D.RAW[, x], na.rm = TRUE)
    colname <- paste(treatmentList$ttotal[j], seasons$name[i], sep = "_")
    # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
    D.RAW <- D.RAW %>% add_column(
      !!(colname) := rsums
    )
    # create a yes no list too
    colnameyn <- paste( treatmentList$ttotal[j], "yn_", seasons$name[i],  sep = "")
    D.RAW <- D.RAW %>% add_column(
      !!(colnameyn) := ifelse(rsums > 0, 1, 0)
    )
    
    
    # Combination
    if(j == 1) next # dont add vcontrol to combination
    vlogical <- as.logical(rsums)
    colshort <- paste(seasons$short[i], treatmentList$tshort[j], sep="-")
    coldesc  <- paste(seasons$desc[i], treatmentList$tname[j], sep=" ")
    
    D.RAW$c_short[vlogical] <- paste(
      D.RAW$c_short[vlogical],
      colshort, 
      sep = " & "
    )
    D.RAW$c_desc[vlogical] <- paste(
      D.RAW$c_desc[vlogical],
      coldesc, 
      sep = " & "
    )
    
  }
}

D.RAW$c_short <- stringr::str_replace(D.RAW$c_short, "NA &", "")
D.RAW$c_desc  <- stringr::str_replace(D.RAW$c_desc, "NA &", "")


rm(seasons, colname, colnameyn, i, j, rsums, treatmentexp, x, vlogical, colshort, coldesc)

#### TOTAL Treatment Values ####

# Temporary for Combinations, without seasons
D.RAW$t_short      <- NA
D.RAW$t_desc       <- NA
D.RAW$t_short_number <- NA

# Loop through our Treatment Types
for(i in 1:nrow(treatmentList)){
  # Get Columns which are starting with List value
  # double blackslash otherwise R wont escape the backslash
  # WE USE HERE all 12 months for spring, summer, winter we only use 01-10 months!
  treatmentexp10 <- paste(
    "(", treatmentList$tsingle[i], ")\\S*0[1-9]|(", treatmentList$tsingle[i], ")\\S*1[0]", 
    sep = ""
  )
  treatmentexp12 <- paste(
    "(", treatmentList$tsingle[i], ")\\S*0[1-9]|(", treatmentList$tsingle[i], ")\\S*1[0-2]", 
    sep = ""
  )
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  x10 <- grepl(treatmentexp10, colnames(D.RAW), fixed = FALSE)
  rsum10 <- rowSums(D.RAW[, x10], na.rm = TRUE)
  x12 <- grepl(treatmentexp12, colnames(D.RAW), fixed = FALSE)
  rsum12 <- rowSums(D.RAW[, x12], na.rm = TRUE)
  
  D.RAW <- D.RAW %>% add_column(
    !!(treatmentList$ttotal[i]) := rsum10,
    !!(paste(treatmentList$ttotal[i], "12", sep="")) := rsum12,
  )
  
  # create a yes (1) no (2) list too
  colnameyn <- paste( treatmentList$ttotal[i], "_yn",  sep = "")
  D.RAW <- D.RAW %>% add_column(
    !!(colnameyn) := ifelse(rsum10 > 0, 1, 0),
  )
  
  # Combination
  if(i == 1) next # dont add vcontrol to combination
  vlogical <- as.logical(rsum12)

  D.RAW$t_short[vlogical] <- paste(
    D.RAW$t_short[vlogical],
    treatmentList$tshort[i], 
    sep = " & "
  )
  D.RAW$t_desc[vlogical] <- paste(
    D.RAW$t_desc[vlogical],
    treatmentList$tname[i], 
    sep = " & "
  )
  
  if(i == 2) next # dont add dronebrood removal to number combination
  
  D.RAW$t_short_number[vlogical] <- paste(
    D.RAW$t_short_number[vlogical],
    paste(rsum12[vlogical], treatmentList$tshort[i], sep="-"), 
    sep = " & "
  )
  
}

D.RAW$t_short <- stringr::str_replace(D.RAW$t_short, "NA &", "")
D.RAW$t_desc  <- stringr::str_replace(D.RAW$t_desc, "NA &", "")
D.RAW$t_short_number  <- stringr::str_replace(D.RAW$t_short_number, "NA &", "")


rm(colnameyn, i, rsum10, rsum12, treatmentexp10, treatmentexp12, x10, x12, vlogical)

# sum rows for total different methods and SEASONS
# sum rows by yn column, that way we get amount of different treatments used
x <- grep("(yn_)", colnames(D.RAW), fixed = FALSE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_totalyn_)", colnames(D.RAW), fixed = FALSE)
x <- x[!(x %in% t)]
D.RAW$T_season <- rowSums(D.RAW[, x], na.rm = TRUE)

# sum rows by yn column, that way we get amount of different treatments used
x <- grep("_yn", colnames(D.RAW), fixed = TRUE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_total_yn)", colnames(D.RAW), fixed = FALSE)
x <- x[!(x %in% t)]
D.RAW$T_amount <- rowSums(D.RAW[, x], na.rm = TRUE)

#### Overrule "wrong" User Input for varroa checked and treated question
D.RAW$varroa_checked[D.RAW$varroa_checked != "Ja" & D.RAW$T_vcount_total12 > 0] <- "Ja"
D.RAW$varroa_treated[D.RAW$varroa_treated != "Ja" & D.RAW$T_amount > 0]         <- "Ja"

rm(t, x)
