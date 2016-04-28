################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### load1.R: load biobank data ###############################

####### SF ###########
SF <- read.csv("raw_data/SF_20160427.csv", stringsAsFactors=FALSE)


###### plasma ###########

## THR ##
THR_PL_BL <- read.csv("raw_data/THR_PL_BL_20160427.csv", stringsAsFactors=FALSE)
THR_PL_6W <- read.csv("raw_data/THR_PL_6W_20160427.csv", stringsAsFactors=FALSE)
THR_PL_3M6M1Y <- read.csv("raw_data/THR_PL_3M6M1Y_20160427.csv", stringsAsFactors=FALSE)

## TKR ##
TKR_PL_BL <- read.csv("raw_data/TKR_PL_BL_20160427.csv", stringsAsFactors=FALSE)
TKR_PL_6W <- read.csv("raw_data/TKR_PL_6W_20160427.csv", stringsAsFactors=FALSE)
TKR_PL_3M6M1Y <- read.csv("raw_data/TKR_PL_3M6M1Y_20160427.csv", stringsAsFactors=FALSE)

## SDF ##
SDF_PL <- read.csv("raw_data/SDF_PL_20160427.csv", stringsAsFactors=FALSE)
