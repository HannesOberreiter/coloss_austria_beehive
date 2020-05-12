### Survey Bee Hive Losses ###
# (c) 2020 Hannes Oberreiter #

# Header File, here we set up basic libaries variables and import the excel

# ---- Clear Enviroment  ----
rm(list=ls()[!ls() %in% c("MAP_AUSTRIA", "MF_DISTRICTS", "MF_STATES")])
# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# ---- Set Packrat Working directory ----
# We are using renv for package management and reproducible
# install.packages("renv")
renv::activate()

# ---- Load Library's ----
library( tidyverse )
library( ggsignif )
library( readxl )
library( gridExtra )
library( boot )
library( rstudioapi ) # to set dirname
library( colorspace ) # better colorpalettes for maps
library( svMisc ) # only for combination function to get a progress bar

#---- Read XLS ----
# Variables and Names
# Our working files, will be imported from excel
FILE.NAME <- "data.xlsx"
SHEET.NAME <- "Winterverluste"
# Load Excel File
V.COL_TYPES_IMPORT <- c(rep("guess", 311))
V.COL_TYPES_IMPORT[12] <- "text" # workaround for ID column which was imported as numeric
D.RAW <- read_excel( FILE.NAME, sheet = SHEET.NAME, skip = 1, col_types = V.COL_TYPES_IMPORT, trim_ws = TRUE)
D.RAW <- D.RAW[, !grepl("[...]", colnames(D.RAW))] # Remove all cols which were empty and auto generated
# Drop NA Rows sometimes error while importing from Excel
D.RAW <- D.RAW[ rowSums( is.na( D.RAW )) != ncol( D.RAW ), ]

# remove contact string and generate simple 0, 1 for data privacy while working on the dataframe
D.RAW$contact = ifelse(is.na( D.RAW$contact ), 0, 1)

#---- Basic Loss Rates ----

# ifelse question to prevent NA Errors and wrong numbers
D.RAW$hives_winter[ is.na( D.RAW$hives_winter )] <- 0
D.RAW$hives_lost[ is.na( D.RAW$hives_lost )] <- 0
D.RAW$lost_a[ is.na( D.RAW$lost_a )] <- 0 # Lost - Queen
D.RAW$lost_b[ is.na( D.RAW$lost_b )] <- 0 # Lost - Elements
D.RAW$lost_c[ is.na( D.RAW$lost_c )] <- 0 # Lost - Other

# Symptomes clean NAs
D.RAW$symp_a[ is.na( D.RAW$symp_a )] <- 0
D.RAW$symp_b[ is.na( D.RAW$symp_b )] <- 0
D.RAW$symp_c[ is.na( D.RAW$symp_c )] <- 0
D.RAW$symp_d[ is.na( D.RAW$symp_d )] <- 0
D.RAW$symp_e[ is.na( D.RAW$symp_e )] <- 0
# sum the symptomes
D.RAW$symp_total = D.RAW$symp_a + D.RAW$symp_b + D.RAW$symp_c + D.RAW$symp_d + D.RAW$symp_e

# Values without loss by elements
D.RAW$hives_lost_e <- ifelse( is.na( D.RAW$lost_b ), D.RAW$hives_lost, D.RAW$hives_lost - D.RAW$lost_b)
D.RAW$hives_spring_e <- D.RAW$hives_winter - D.RAW$hives_lost_e	
# Values for Queens
D.RAW$hives_spring_queen <- ifelse( is.na( D.RAW$lost_a ), NA, D.RAW$hives_winter - D.RAW$lost_a )
# Loss rate per company
D.RAW$lost_rate <- D.RAW$hives_lost / D.RAW$hives_winter * 100
D.RAW$lost_rate_e <- D.RAW$hives_lost_e / D.RAW$hives_winter * 100
# hives per apiary
D.RAW$hives_per_apiary <- D.RAW$hives_winter / D.RAW$apiaries
# Type of submitting
D.RAW$submitted <- "Internet"
D.RAW$submitted[grepl("P", D.RAW$id, fixed = TRUE)] <- 'Papier'
D.RAW$submitted[grepl("Z", D.RAW$id, fixed = TRUE)] <- 'Zeitung'

# ---- Remove Temporary -----
rm(FILE.NAME, SHEET.NAME, V.COL_TYPES_IMPORT)


