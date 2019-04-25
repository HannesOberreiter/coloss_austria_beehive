##############################
### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #
##############################
##############################

# Header File, here we set up basic libaries variables and import the excel

# Clear Enviroment
rm(list=ls())

# Load Library's
###############
library( ggplot2 )
library( gridExtra )
library( grid )
library( readxl )
library( reshape2 )
library( dplyr )
library( tidyr )
library( scales )
#library( Rcmdr )
library( boot )

# Read XLS
###############
# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Variables and Names
###############
# Our working files, will be imported from excel
FILE.NAME <- "Daten.xlsx"
SHEET.NAME <- "Winterverluste"
# Load Excel File
D.FULL <- read_excel( FILE.NAME, sheet = SHEET.NAME, skip = 5 ) #Skip first 5 rows, as they are empty
# Drop NA Rows sometimes error while importing from Excel
D.FULL <- D.FULL[ rowSums( is.na( D.FULL )) != ncol( D.FULL ), ]

# Remove Columns we dont want/need
cremove <- c(
  "https://geocode.localfocus.nl/", 
  "eingew<Loss", 
  "eingew<Loss+Schwach", 
  "MaxSympt", 
  "Datum Abgeschickt", 
  "Letzte Seite", 
  "Zufallsgeneratorstartwert",
  "Start-Sprache",
  "Datum letzte Aktivität",
  "Datum gestartet",
  "IP-Adresse",
  "Weiterleitungs-URL",
  "Kontakt"
  )
# One of then it works even when the column is not in the file
D.FULL <- select(D.FULL, -one_of( cremove ))

# Change names to be more efficient and readable
D.FULL <- D.FULL %>%
  
  # Main Columns
  rename( "lost_a" = "[a) ... weisellos oder drohnenbrütig ?]" ) %>%
  rename( "lost_b" = "[b) ... verloren durch Elementarschaden (Flut, Tierschaden, Vandalismus, etc.)?]" ) %>%
  rename( "lost_c" = "[c) ... verloren (tote Völker, leere Beuten)?]" ) %>%
  rename( "hives_winter" = "Wie viele Bienenvölker haben Sie 2018 eingewintert?" ) %>%
  rename( "hives_lost" = "Verlust sumloss" ) %>%
  
  # Operational Factors
  rename( "migratory_beekeeper" = "transportiert-" ) %>%
  rename( "mash_bottom_board" = "Offener Gitterboden im Winter]" ) %>%
  rename( "insulated_hives" = "Isolierte Beuten im Winter]" ) %>%
  rename( "plastic_hives" = "Kunststoff-Beuten]" ) %>%
  rename( "cert_org_beek" = "Zertifizierte Bio-Imkerei]" ) %>%
  rename( "varroatolerant" = "Zucht auf Varroatoleranz]" ) %>%
  rename( "small_broodcells" = "Kleine Brutzellen (5,1 mm oder weniger)]" ) %>%
  rename( "no_foundation" = "Naturwabenbau (ohne Mittelwand)]" ) %>%
  rename( "foreign_wax" = "Kaufe Wachs zu (kein eigener Wachskreislauf)]" ) %>%
  rename( "varroa_checked" = "Haben Sie im Zeitraum April 2018 bis April 2019 den Varroa-Befall Ihrer Völker bestimmt?  []") %>%
  rename( "varroa_treated" = "Haben Sie im Zeitraum April 2018 - April 2019 Ihre Völker gegen Varroa behandelt?  []") %>%
  rename( "new_frames" = "Brutwaben Ihrer Völker haben Sie 2018 erneuert?") %>%
  
  # Yield Factors
  rename( "brassica_napus" = "Raps]") %>%
  rename( "zea_mays" = "Mais]") %>%
  rename( "helianthus_annuus" = "Sonnenblume]") %>%
  rename( "late_catch_crop" = "Spätblühende Zwischenfrüchte]") %>%
  rename( "honeydew" = "Waldtracht]") %>%
  rename( "melezitose" = "Waldtracht mit Melezitose]") %>%
  
  # Queen Columns  
  rename( "queen_problems" = "Königinnen-Probleme bemerkt (verglichen mit Ihren bisherigen Erfahrungen)-") %>%
  rename( "young_queens" = "\"junge\" Königin-" ) %>%
  
  ## Treatment
  # Drone Comb removal
  rename( "T_drone_01" = "[Drohnenbrutentnahme][Apr 17]" ) %>%
  rename( "T_drone_02" = "[Drohnenbrutentnahme][Mai 17]" ) %>%
  rename( "T_drone_03" = "[Drohnenbrutentnahme][Jun 17]" ) %>%
  rename( "T_drone_04" = "[Drohnenbrutentnahme][Jul 17]" ) %>%
  rename( "T_drone_05" = "[Drohnenbrutentnahme][Aug 17]" ) %>%
  rename( "T_drone_06" = "[Drohnenbrutentnahme][Sep 17]" ) %>%
  rename( "T_drone_07" = "[Drohnenbrutentnahme][Okt 17]" ) %>%
  rename( "T_drone_08" = "[Drohnenbrutentnahme][Nov 17]" ) %>%
  rename( "T_drone_09" = "[Drohnenbrutentnahme][Dez 17]" ) %>%
  rename( "T_drone_10" = "[Drohnenbrutentnahme][Jän 18]" ) %>%
  rename( "T_drone_11" = "[Drohnenbrutentnahme][Feb 18]" ) %>%
  rename( "T_drone_12" = "[Drohnenbrutentnahme][Mär 18]" ) %>%
  rename( "T_drone_13" = "[Drohnenbrutentnahme][Apr 18]" ) %>%
  # Hyperthermia
  rename( "T_hyperthermia_01" = "[Hyperthermie (Hitzebehandlung)][Apr 17]" ) %>%
  rename( "T_hyperthermia_02" = "[Hyperthermie (Hitzebehandlung)][Mai 17]" ) %>%
  rename( "T_hyperthermia_03" = "[Hyperthermie (Hitzebehandlung)][Jun 17]" ) %>%
  rename( "T_hyperthermia_04" = "[Hyperthermie (Hitzebehandlung)][Jul 17]" ) %>%
  rename( "T_hyperthermia_05" = "[Hyperthermie (Hitzebehandlung)][Aug 17]" ) %>%
  rename( "T_hyperthermia_06" = "[Hyperthermie (Hitzebehandlung)][Sep 17]" ) %>%
  rename( "T_hyperthermia_07" = "[Hyperthermie (Hitzebehandlung)][Okt 17]" ) %>%
  rename( "T_hyperthermia_08" = "[Hyperthermie (Hitzebehandlung)][Nov 17]" ) %>%
  rename( "T_hyperthermia_09" = "[Hyperthermie (Hitzebehandlung)][Dez 17]" ) %>%
  rename( "T_hyperthermia_10" = "[Hyperthermie (Hitzebehandlung)][Jän 18]" ) %>%
  rename( "T_hyperthermia_11" = "[Hyperthermie (Hitzebehandlung)][Feb 18]" ) %>%
  rename( "T_hyperthermia_12" = "[Hyperthermie (Hitzebehandlung)][Mär 18]" ) %>%
  rename( "T_hyperthermia_13" = "[Hyperthermie (Hitzebehandlung)][Apr 18]" ) %>%
  # Biotechnical
  rename( "T_biotechnical_01" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Apr 17]" ) %>%
  rename( "T_biotechnical_02" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Mai 17]" ) %>%
  rename( "T_biotechnical_03" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Jun 17]" ) %>%
  rename( "T_biotechnical_04" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Jul 17]" ) %>%
  rename( "T_biotechnical_05" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Aug 17]" ) %>%
  rename( "T_biotechnical_06" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Sep 17]" ) %>%
  rename( "T_biotechnical_07" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Okt 17]" ) %>%
  rename( "T_biotechnical_08" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Nov 17]" ) %>%
  rename( "T_biotechnical_09" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Dez 17]" ) %>%
  rename( "T_biotechnical_10" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Jän 18]" ) %>%
  rename( "T_biotechnical_11" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Feb 18]" ) %>%
  rename( "T_biotechnical_12" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Mär 18]" ) %>%
  rename( "T_biotechnical_13" = "[Andere biotechnische Methode (Fangwabe, Bannwabe,  totale Arbeiterinnen-Brutentnahme etc.)][Apr 18]" ) %>%
  # Formic Acid Short
  rename( "T_formic_short_01" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Apr 17]" ) %>%
  rename( "T_formic_short_02" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Mai 17]" ) %>%
  rename( "T_formic_short_03" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Jun 17]" ) %>%
  rename( "T_formic_short_04" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Jul 17]" ) %>%
  rename( "T_formic_short_05" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Aug 17]" ) %>%
  rename( "T_formic_short_06" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Sep 17]" ) %>%
  rename( "T_formic_short_07" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Okt 17]" ) %>%
  rename( "T_formic_short_08" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Nov 17]" ) %>%
  rename( "T_formic_short_09" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Dez 17]" ) %>%
  rename( "T_formic_short_10" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Jän 18]" ) %>%
  rename( "T_formic_short_11" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Feb 18]" ) %>%
  rename( "T_formic_short_12" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Mär 18]" ) %>%
  rename( "T_formic_short_13" = "[Ameisensäure Kurzzeitbehandlung (inkl. MAQS)][Apr 18]" ) %>%
  # Formic Acid Long
  rename( "T_formic_long_01" = "[Ameisensäure Langzeitbehandlung][Apr 17]" ) %>%
  rename( "T_formic_long_02" = "[Ameisensäure Langzeitbehandlung][Mai 17]" ) %>%
  rename( "T_formic_long_03" = "[Ameisensäure Langzeitbehandlung][Jun 17]" ) %>%
  rename( "T_formic_long_04" = "[Ameisensäure Langzeitbehandlung][Jul 17]" ) %>%
  rename( "T_formic_long_05" = "[Ameisensäure Langzeitbehandlung][Aug 17]" ) %>%
  rename( "T_formic_long_06" = "[Ameisensäure Langzeitbehandlung][Sep 17]" ) %>%
  rename( "T_formic_long_07" = "[Ameisensäure Langzeitbehandlung][Okt 17]" ) %>%
  rename( "T_formic_long_08" = "[Ameisensäure Langzeitbehandlung][Nov 17]" ) %>%
  rename( "T_formic_long_09" = "[Ameisensäure Langzeitbehandlung][Dez 17]" ) %>%
  rename( "T_formic_long_10" = "[Ameisensäure Langzeitbehandlung][Jän 18]" ) %>%
  rename( "T_formic_long_11" = "[Ameisensäure Langzeitbehandlung][Feb 18]" ) %>%
  rename( "T_formic_long_12" = "[Ameisensäure Langzeitbehandlung][Mär 18]" ) %>%
  rename( "T_formic_long_13" = "[Ameisensäure Langzeitbehandlung][Apr 18]" ) %>%
  # Lactic Acid
  rename( "T_lactic_01" = "[Milchsäure][Apr 17]" ) %>%
  rename( "T_lactic_02" = "[Milchsäure][Mai 17]" ) %>%
  rename( "T_lactic_03" = "[Milchsäure][Jun 17]" ) %>%
  rename( "T_lactic_04" = "[Milchsäure][Jul 17]" ) %>%
  rename( "T_lactic_05" = "[Milchsäure][Aug 17]" ) %>%
  rename( "T_lactic_06" = "[Milchsäure][Sep 17]" ) %>%
  rename( "T_lactic_07" = "[Milchsäure][Okt 17]" ) %>%
  rename( "T_lactic_08" = "[Milchsäure][Nov 17]" ) %>%
  rename( "T_lactic_09" = "[Milchsäure][Dez 17]" ) %>%
  rename( "T_lactic_10" = "[Milchsäure][Jän 18]" ) %>%
  rename( "T_lactic_11" = "[Milchsäure][Feb 18]" ) %>%
  rename( "T_lactic_12" = "[Milchsäure][Mär 18]" ) %>%
  rename( "T_lactic_13" = "[Milchsäure][Apr 18]" ) %>%
  # Oxalic Acid - Trickle
  rename( "T_oxalic_trickle_01" = "[Oxalsäure Träufeln (oder Sprühen)][Apr 17]" ) %>%
  rename( "T_oxalic_trickle_02" = "[Oxalsäure Träufeln (oder Sprühen)][Mai 17]" ) %>%
  rename( "T_oxalic_trickle_03" = "[Oxalsäure Träufeln (oder Sprühen)][Jun 17]" ) %>%
  rename( "T_oxalic_trickle_04" = "[Oxalsäure Träufeln (oder Sprühen)][Jul 17]" ) %>%
  rename( "T_oxalic_trickle_05" = "[Oxalsäure Träufeln (oder Sprühen)][Aug 17]" ) %>%
  rename( "T_oxalic_trickle_06" = "[Oxalsäure Träufeln (oder Sprühen)][Sep 17]" ) %>%
  rename( "T_oxalic_trickle_07" = "[Oxalsäure Träufeln (oder Sprühen)][Okt 17]" ) %>%
  rename( "T_oxalic_trickle_08" = "[Oxalsäure Träufeln (oder Sprühen)][Nov 17]" ) %>%
  rename( "T_oxalic_trickle_09" = "[Oxalsäure Träufeln (oder Sprühen)][Dez 17]" ) %>%
  rename( "T_oxalic_trickle_10" = "[Oxalsäure Träufeln (oder Sprühen)][Jän 18]" ) %>%
  rename( "T_oxalic_trickle_11" = "[Oxalsäure Träufeln (oder Sprühen)][Feb 18]" ) %>%
  rename( "T_oxalic_trickle_12" = "[Oxalsäure Träufeln (oder Sprühen)][Mär 18]" ) %>%
  rename( "T_oxalic_trickle_13" = "[Oxalsäure Träufeln (oder Sprühen)][Apr 18]" ) %>%
  # Oxalic Acid - Vaporize
  rename( "T_oxalic_vapo_01" = "[Oxalsäure Verdampfen][Apr 17]" ) %>%
  rename( "T_oxalic_vapo_02" = "[Oxalsäure Verdampfen][Mai 17]" ) %>%
  rename( "T_oxalic_vapo_03" = "[Oxalsäure Verdampfen][Jun 17]" ) %>%
  rename( "T_oxalic_vapo_04" = "[Oxalsäure Verdampfen][Jul 17]" ) %>%
  rename( "T_oxalic_vapo_05" = "[Oxalsäure Verdampfen][Aug 17]" ) %>%
  rename( "T_oxalic_vapo_06" = "[Oxalsäure Verdampfen][Sep 17]" ) %>%
  rename( "T_oxalic_vapo_07" = "[Oxalsäure Verdampfen][Okt 17]" ) %>%
  rename( "T_oxalic_vapo_08" = "[Oxalsäure Verdampfen][Nov 17]" ) %>%
  rename( "T_oxalic_vapo_09" = "[Oxalsäure Verdampfen][Dez 17]" ) %>%
  rename( "T_oxalic_vapo_10" = "[Oxalsäure Verdampfen][Jän 18]" ) %>%
  rename( "T_oxalic_vapo_11" = "[Oxalsäure Verdampfen][Feb 18]" ) %>%
  rename( "T_oxalic_vapo_12" = "[Oxalsäure Verdampfen][Mär 18]" ) %>%
  rename( "T_oxalic_vapo_13" = "[Oxalsäure Verdampfen][Apr 18]" ) %>%
  # Oxalic Acid - Mixture
  rename( "T_oxalic_mix_01" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Apr 17]" ) %>%
  rename( "T_oxalic_mix_02" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Mai 17]" ) %>%
  rename( "T_oxalic_mix_03" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Jun 17]" ) %>%
  rename( "T_oxalic_mix_04" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Jul 17]" ) %>%
  rename( "T_oxalic_mix_05" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Aug 17]" ) %>%
  rename( "T_oxalic_mix_06" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Sep 17]" ) %>%
  rename( "T_oxalic_mix_07" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Okt 17]" ) %>%
  rename( "T_oxalic_mix_08" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Nov 17]" ) %>%
  rename( "T_oxalic_mix_09" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Dez 17]" ) %>%
  rename( "T_oxalic_mix_10" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Jän 18]" ) %>%
  rename( "T_oxalic_mix_11" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Feb 18]" ) %>%
  rename( "T_oxalic_mix_12" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Mär 18]" ) %>%
  rename( "T_oxalic_mix_13" = "[Oxalsäuremischungen (Hiveclean/Bienenwohl/Varromed)][Apr 18]" ) %>%
  # Thymol
  rename( "T_thymol_01" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Apr 17]" ) %>%
  rename( "T_thymol_02" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Mai 17]" ) %>%
  rename( "T_thymol_03" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Jun 17]" ) %>%
  rename( "T_thymol_04" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Jul 17]" ) %>%
  rename( "T_thymol_05" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Aug 17]" ) %>%
  rename( "T_thymol_06" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Sep 17]" ) %>%
  rename( "T_thymol_07" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Okt 17]" ) %>%
  rename( "T_thymol_08" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Nov 17]" ) %>%
  rename( "T_thymol_09" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Dez 17]" ) %>%
  rename( "T_thymol_10" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Jän 18]" ) %>%
  rename( "T_thymol_11" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Feb 18]" ) %>%
  rename( "T_thymol_12" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Mär 18]" ) %>%
  rename( "T_thymol_13" = "[Thymol (Apiguard, Apilife VAR, Thymovar)][Apr 18]" ) %>%
  # Tau-fluvalinat
  rename( "T_synthetic_apistan_01" = "[Tau-fluvalinat (Apistan)][Apr 17]" ) %>%
  rename( "T_synthetic_apistan_02" = "[Tau-fluvalinat (Apistan)][Mai 17]" ) %>%
  rename( "T_synthetic_apistan_03" = "[Tau-fluvalinat (Apistan)][Jun 17]" ) %>%
  rename( "T_synthetic_apistan_04" = "[Tau-fluvalinat (Apistan)][Jul 17]" ) %>%
  rename( "T_synthetic_apistan_05" = "[Tau-fluvalinat (Apistan)][Aug 17]" ) %>%
  rename( "T_synthetic_apistan_06" = "[Tau-fluvalinat (Apistan)][Sep 17]" ) %>%
  rename( "T_synthetic_apistan_07" = "[Tau-fluvalinat (Apistan)][Okt 17]" ) %>%
  rename( "T_synthetic_apistan_08" = "[Tau-fluvalinat (Apistan)][Nov 17]" ) %>%
  rename( "T_synthetic_apistan_09" = "[Tau-fluvalinat (Apistan)][Dez 17]" ) %>%
  rename( "T_synthetic_apistan_10" = "[Tau-fluvalinat (Apistan)][Jän 18]" ) %>%
  rename( "T_synthetic_apistan_11" = "[Tau-fluvalinat (Apistan)][Feb 18]" ) %>%
  rename( "T_synthetic_apistan_12" = "[Tau-fluvalinat (Apistan)][Mär 18]" ) %>%
  rename( "T_synthetic_apistan_13" = "[Tau-fluvalinat (Apistan)][Apr 18]" ) %>%
  # Flumethrin
  rename( "T_synthetic_flumethrin_01" = "[Flumethrin (Bayvarol, Polyvar)][Apr 17]" ) %>%
  rename( "T_synthetic_flumethrin_02" = "[Flumethrin (Bayvarol, Polyvar)][Mai 17]" ) %>%
  rename( "T_synthetic_flumethrin_03" = "[Flumethrin (Bayvarol, Polyvar)][Jun 17]" ) %>%
  rename( "T_synthetic_flumethrin_04" = "[Flumethrin (Bayvarol, Polyvar)][Jul 17]" ) %>%
  rename( "T_synthetic_flumethrin_05" = "[Flumethrin (Bayvarol, Polyvar)][Aug 17]" ) %>%
  rename( "T_synthetic_flumethrin_06" = "[Flumethrin (Bayvarol, Polyvar)][Sep 17]" ) %>%
  rename( "T_synthetic_flumethrin_07" = "[Flumethrin (Bayvarol, Polyvar)][Okt 17]" ) %>%
  rename( "T_synthetic_flumethrin_08" = "[Flumethrin (Bayvarol, Polyvar)][Nov 17]" ) %>%
  rename( "T_synthetic_flumethrin_09" = "[Flumethrin (Bayvarol, Polyvar)][Dez 17]" ) %>%
  rename( "T_synthetic_flumethrin_10" = "[Flumethrin (Bayvarol, Polyvar)][Jän 18]" ) %>%
  rename( "T_synthetic_flumethrin_11" = "[Flumethrin (Bayvarol, Polyvar)][Feb 18]" ) %>%
  rename( "T_synthetic_flumethrin_12" = "[Flumethrin (Bayvarol, Polyvar)][Mär 18]" ) %>%
  rename( "T_synthetic_flumethrin_13" = "[Flumethrin (Bayvarol, Polyvar)][Apr 18]" ) %>%
  # Amitraz - strips
  rename( "T_synthetic_amitraz_strips_01" = "[Amitraz (in Streifen, Apivar, Apitraz)][Apr 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_02" = "[Amitraz (in Streifen, Apivar, Apitraz)][Mai 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_03" = "[Amitraz (in Streifen, Apivar, Apitraz)][Jun 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_04" = "[Amitraz (in Streifen, Apivar, Apitraz)][Jul 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_05" = "[Amitraz (in Streifen, Apivar, Apitraz)][Aug 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_06" = "[Amitraz (in Streifen, Apivar, Apitraz)][Sep 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_07" = "[Amitraz (in Streifen, Apivar, Apitraz)][Okt 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_08" = "[Amitraz (in Streifen, Apivar, Apitraz)][Nov 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_09" = "[Amitraz (in Streifen, Apivar, Apitraz)][Dez 17]" ) %>%
  rename( "T_synthetic_amitraz_strips_10" = "[Amitraz (in Streifen, Apivar, Apitraz)][Jän 18]" ) %>%
  rename( "T_synthetic_amitraz_strips_11" = "[Amitraz (in Streifen, Apivar, Apitraz)][Feb 18]" ) %>%
  rename( "T_synthetic_amitraz_strips_12" = "[Amitraz (in Streifen, Apivar, Apitraz)][Mär 18]" ) %>%
  rename( "T_synthetic_amitraz_strips_13" = "[Amitraz (in Streifen, Apivar, Apitraz)][Apr 18]" ) %>%  
  # Amitraz - Vaporize
  rename( "T_synthetic_amitraz_vapo_01" = "[Amitraz (Verdampfen)][Apr 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_02" = "[Amitraz (Verdampfen)][Mai 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_03" = "[Amitraz (Verdampfen)][Jun 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_04" = "[Amitraz (Verdampfen)][Jul 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_05" = "[Amitraz (Verdampfen)][Aug 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_06" = "[Amitraz (Verdampfen)][Sep 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_07" = "[Amitraz (Verdampfen)][Okt 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_08" = "[Amitraz (Verdampfen)][Nov 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_09" = "[Amitraz (Verdampfen)][Dez 17]" ) %>%
  rename( "T_synthetic_amitraz_vapo_10" = "[Amitraz (Verdampfen)][Jän 18]" ) %>%
  rename( "T_synthetic_amitraz_vapo_11" = "[Amitraz (Verdampfen)][Feb 18]" ) %>%
  rename( "T_synthetic_amitraz_vapo_12" = "[Amitraz (Verdampfen)][Mär 18]" ) %>%
  rename( "T_synthetic_amitraz_vapo_13" = "[Amitraz (Verdampfen)][Apr 18]" ) %>%  
  # Coumaphos - Perizin
  rename( "T_synthetic_coumaphos_p_01" = "[Coumaphos (Perizin)][Apr 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_02" = "[Coumaphos (Perizin)][Mai 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_03" = "[Coumaphos (Perizin)][Jun 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_04" = "[Coumaphos (Perizin)][Jul 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_05" = "[Coumaphos (Perizin)][Aug 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_06" = "[Coumaphos (Perizin)][Sep 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_07" = "[Coumaphos (Perizin)][Okt 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_08" = "[Coumaphos (Perizin)][Nov 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_09" = "[Coumaphos (Perizin)][Dez 17]" ) %>%
  rename( "T_synthetic_coumaphos_p_10" = "[Coumaphos (Perizin)][Jän 18]" ) %>%
  rename( "T_synthetic_coumaphos_p_11" = "[Coumaphos (Perizin)][Feb 18]" ) %>%
  rename( "T_synthetic_coumaphos_p_12" = "[Coumaphos (Perizin)][Mär 18]" ) %>%
  rename( "T_synthetic_coumaphos_p_13" = "[Coumaphos (Perizin)][Apr 18]" ) %>%  
  # Coumaphos - Checkmite
  rename( "T_synthetic_coumaphos_c_01" = "[Coumaphos (Checkmite+)][Apr 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_02" = "[Coumaphos (Checkmite+)][Mai 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_03" = "[Coumaphos (Checkmite+)][Jun 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_04" = "[Coumaphos (Checkmite+)][Jul 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_05" = "[Coumaphos (Checkmite+)][Aug 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_06" = "[Coumaphos (Checkmite+)][Sep 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_07" = "[Coumaphos (Checkmite+)][Okt 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_08" = "[Coumaphos (Checkmite+)][Nov 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_09" = "[Coumaphos (Checkmite+)][Dez 17]" ) %>%
  rename( "T_synthetic_coumaphos_c_10" = "[Coumaphos (Checkmite+)][Jän 18]" ) %>%
  rename( "T_synthetic_coumaphos_c_11" = "[Coumaphos (Checkmite+)][Feb 18]" ) %>%
  rename( "T_synthetic_coumaphos_c_12" = "[Coumaphos (Checkmite+)][Mär 18]" ) %>%
  rename( "T_synthetic_coumaphos_c_13" = "[Coumaphos (Checkmite+)][Apr 18]" ) %>%  
  # Other synthetic
  rename( "T_synthetic_synother_01" = "[Anderes chemisches Produkt][Apr 17]" ) %>%
  rename( "T_synthetic_synother_02" = "[Anderes chemisches Produkt][Mai 17]" ) %>%
  rename( "T_synthetic_synother_03" = "[Anderes chemisches Produkt][Jun 17]" ) %>%
  rename( "T_synthetic_synother_04" = "[Anderes chemisches Produkt][Jul 17]" ) %>%
  rename( "T_synthetic_synother_05" = "[Anderes chemisches Produkt][Aug 17]" ) %>%
  rename( "T_synthetic_synother_06" = "[Anderes chemisches Produkt][Sep 17]" ) %>%
  rename( "T_synthetic_synother_07" = "[Anderes chemisches Produkt][Okt 17]" ) %>%
  rename( "T_synthetic_synother_08" = "[Anderes chemisches Produkt][Nov 17]" ) %>%
  rename( "T_synthetic_synother_09" = "[Anderes chemisches Produkt][Dez 17]" ) %>%
  rename( "T_synthetic_synother_10" = "[Anderes chemisches Produkt][Jän 18]" ) %>%
  rename( "T_synthetic_synother_11" = "[Anderes chemisches Produkt][Feb 18]" ) %>%
  rename( "T_synthetic_synother_12" = "[Anderes chemisches Produkt][Mär 18]" ) %>%
  rename( "T_synthetic_synother_13" = "[Anderes chemisches Produkt][Apr 18]" ) %>%  
  # Other
  rename( "T_other_01" = "[Andere Methode][Apr 17]" ) %>%
  rename( "T_other_02" = "[Andere Methode][Mai 17]" ) %>%
  rename( "T_other_03" = "[Andere Methode][Jun 17]" ) %>%
  rename( "T_other_04" = "[Andere Methode][Jul 17]" ) %>%
  rename( "T_other_05" = "[Andere Methode][Aug 17]" ) %>%
  rename( "T_other_06" = "[Andere Methode][Sep 17]" ) %>%
  rename( "T_other_07" = "[Andere Methode][Okt 17]" ) %>%
  rename( "T_other_08" = "[Andere Methode][Nov 17]" ) %>%
  rename( "T_other_09" = "[Andere Methode][Dez 17]" ) %>%
  rename( "T_other_10" = "[Andere Methode][Jän 18]" ) %>%
  rename( "T_other_11" = "[Andere Methode][Feb 18]" ) %>%
  rename( "T_other_12" = "[Andere Methode][Mär 18]" ) %>%
  rename( "T_other_13" = "[Andere Methode][Apr 18]" ) %>%  
  
  # Other
  rename( "apiaries" = "Wie viele Bienenstände (Standorte) haben Sie?" )

# Basic Code
###############
# Order our states
D.FULL$Bundesland <- factor( D.FULL$Bundesland, 
                             levels = c( "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien"))

# Clear N/A Errors while calculating
D.FULL$lost_a[is.na(D.FULL$lost_a)] <- 0
D.FULL$lost_b[is.na(D.FULL$lost_b)] <- 0
D.FULL$lost_c[is.na(D.FULL$lost_c)] <- 0

# Add Spring hive amount
D.FULL$hives_spring <- D.FULL$hives_winter - D.FULL$hives_lost
# Values without loss by elements
D.FULL$hives_lost_e <- D.FULL$hives_lost - D.FULL$lost_b
D.FULL$hives_spring_e <- D.FULL$hives_winter - D.FULL$hives_lost_e
# Values for Queens
D.FULL$hives_spring_queen <- D.FULL$hives_winter - D.FULL$lost_a
# Loss rate per company
D.FULL$lost_rate <- D.FULL$hives_lost / D.FULL$hives_winter * 100
D.FULL$lost_rate_e <- D.FULL$hives_lost_e / D.FULL$hives_winter * 100
# hives per apiary
D.FULL$hives_per_apiary <- D.FULL$hives_winter / D.FULL$apiaries

