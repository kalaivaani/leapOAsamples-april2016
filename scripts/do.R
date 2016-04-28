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

#### load1.R: load biobank export data ####
source("scripts/load1.R")

#### merge.R: merge together biobank datasets ####
source("scripts/merge.R")

#### load2.R: load biobank summary data and DADOS data ####
source("scripts/load2.R")

#### clean.R: clean data, subset and derive variables ####
source("scripts/clean.R")

#### analysis.R: generate metrics ####
source("scripts/analysis.R")
