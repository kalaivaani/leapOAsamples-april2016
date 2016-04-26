################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### do.R: load, clean/merge, and generate summary data ###############################
rm(list = ls())
library(plyr)
library(lubridate)

#### load functions ####
source("Y:/LEAP/23. LEAP OA Data Quality/scripts/functions.R")

#### load.R: load data ####
source("scripts/load.R")

#### merge.R: merge together appropriate datasets ####
source("scripts/merge.R")

#### clean.R: clean dataset, subset and derive variables ####
source("scripts/clean.R")
