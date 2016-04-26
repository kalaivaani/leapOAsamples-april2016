################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### load2.R: load biobank summary data and DADOS data ###############################
rm(list = ls())
library(plyr)
library(lubridate)

#### load functions ####
source("Y:/LEAP/23. LEAP OA Data Quality/scripts/functions.R")

### biobank summaries
load("processed_data/PL.Rdata")
load("processed_data/SF.Rdata")


#### DADOS data ####
# master list
LEAPOA_master <- read.csv("raw_data/OA_master_20160426.csv", stringsAsFactors=FALSE)

# Core surveys
Core_Hip <- read.csv("raw_data/Core_Hip_surveys_20160426.csv", stringsAsFactors=FALSE)
Core_Knee <- read.csv("raw_data/Core_Knee_surveys_20160426.csv", stringsAsFactors=FALSE)
Core_Spine <- read.csv("raw_data/Core_Spine_surveys_20160426.csv", stringsAsFactors=FALSE)

# OA surveys
OA_Hip <- read.csv("raw_data/OA_Hip_surveys_20160426.csv", stringsAsFactors=FALSE)
OA_Knee <- read.csv("raw_data/OA_Knee_surveys_20160426.csv", stringsAsFactors=FALSE)
OA_Spine <- read.csv("raw_data/OA_Spine_surveys_20160426.csv", stringsAsFactors=FALSE)
