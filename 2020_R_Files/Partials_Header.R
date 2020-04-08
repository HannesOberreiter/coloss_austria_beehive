### Survey Bee Hive Losses ###
# (c) 2019 Hannes Oberreiter #

# Header File, here we set up basic libaries variables and import the excel

# ---- Clear Enviroment  ----
rm(list=ls())

# ---- Load Library's ----
library( ggplot2 )
library( ggrepel )
library( gridExtra )
library( grid )
library( readxl )
library( reshape2 )
library( dplyr )
library( tidyr )
library( scales )
#library( Rcmdr )
library( boot )
library( rstudioapi ) # to set dirname
library( colorspace ) # better colorpalettes for maps
library( svMisc ) # only for combination function to get a progress bar

#---- Read XLS ----
# Set Working directory (uses API of RStudio)
SCRIPT.DIR <- dirname( rstudioapi::getActiveDocumentContext()$path )
setwd( SCRIPT.DIR )

# Variables and Names
# Our working files, will be imported from excel
FILE.NAME <- "data.xlsx"
SHEET.NAME <- "Winterverluste"
# Load Excel File
D.RAW <- read_excel( FILE.NAME, sheet = SHEET.NAME, skip = 1 ) #Skip first 5 rows, as it is the original names
D.RAW <- D.RAW[, !grepl("[0-9]", colnames(D.RAW))] # Remove all cols which were empty and auto generated
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

# Add Spring hive amount
D.RAW$hives_spring <- D.RAW$hives_winter - D.RAW$hives_lost
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
# hive production this season before winter
D.RAW$hives_production <- D.RAW$hives_winter - D.RAW$hives_spring_before


# ---- Remove Temporary -----
rm(FILE.NAME, SHEET.NAME)


