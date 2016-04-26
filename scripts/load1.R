################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### load1.R: load biobank data ###############################

## THR ##
# SF
THR_SF <- read.csv("raw_data/THR_SF_20160420.csv", stringsAsFactors=FALSE)
THR_SF_QUANTITY <- read.csv("raw_data/THR_SF_QUANTITY_20160425.csv", stringsAsFactors=FALSE)

# plasma
THR_PL_BL <- read.csv("raw_data/THR_PL_BL_20160422.csv", stringsAsFactors=FALSE)
THR_PL_6W <- read.csv("raw_data/THR_PL_6W_20160422.csv", stringsAsFactors=FALSE)
THR_PL_3M <- read.csv("raw_data/THR_PL_3M_20160422.csv", stringsAsFactors=FALSE)
THR_PL_6M <- read.csv("raw_data/THR_PL_6M_20160422.csv", stringsAsFactors=FALSE)
THR_PL_1Y <- read.csv("raw_data/THR_PL_1Y_20160422.csv", stringsAsFactors=FALSE)

THR_PL_Q_BL <- read.csv("raw_data/THR_PL_QUAL_BL_20160425.csv", stringsAsFactors=FALSE)
THR_PL_Q_6W <- read.csv("raw_data/THR_PL_QUAL_6W_20160425.csv", stringsAsFactors=FALSE)
THR_PL_Q_3M <- read.csv("raw_data/THR_PL_QUAL_3M_20160425.csv", stringsAsFactors=FALSE)
THR_PL_Q_6M <- read.csv("raw_data/THR_PL_QUAL_6M_20160425.csv", stringsAsFactors=FALSE)
THR_PL_Q_1Y <- read.csv("raw_data/THR_PL_QUAL_1Y_20160425.csv", stringsAsFactors=FALSE)



## TKR ##
# SF
TKR_SF <- read.csv("raw_data/TKR_SF_20160420.csv", stringsAsFactors=FALSE)
TKR_SF_QUANTITY <- read.csv("raw_data/TKR_SF_QUANTITY_20160425.csv", stringsAsFactors=FALSE)

# plasma
TKR_PL_BL <- read.csv("raw_data/TKR_PL_BL_20160422.csv", stringsAsFactors=FALSE)
TKR_PL_6W <- read.csv("raw_data/TKR_PL_6W_20160422.csv", stringsAsFactors=FALSE)
TKR_PL_3M <- read.csv("raw_data/TKR_PL_3M_20160422.csv", stringsAsFactors=FALSE)
TKR_PL_6M <- read.csv("raw_data/TKR_PL_6M_20160422.csv", stringsAsFactors=FALSE)
TKR_PL_1Y <- read.csv("raw_data/TKR_PL_1Y_20160422.csv", stringsAsFactors=FALSE)

TKR_PL_Q_BL <- read.csv("raw_data/TKR_PL_QUAL_BL_20160425.csv", stringsAsFactors=FALSE)
TKR_PL_Q_6W <- read.csv("raw_data/TKR_PL_QUAL_6W_20160425.csv", stringsAsFactors=FALSE)
TKR_PL_Q_3M <- read.csv("raw_data/TKR_PL_QUAL_3M_20160425.csv", stringsAsFactors=FALSE)
TKR_PL_Q_6M <- read.csv("raw_data/TKR_PL_QUAL_6M_20160425.csv", stringsAsFactors=FALSE)
TKR_PL_Q_1Y <- read.csv("raw_data/TKR_PL_QUAL_1Y_20160425.csv", stringsAsFactors=FALSE)



## SDF ##
# plasma
SDF_PL <- read.csv("raw_data/SDF_PL_20160422.csv", stringsAsFactors=FALSE)
SDF_PL_Q <- read.csv("raw_data/SDF_PL_QUAL_20160425.csv", stringsAsFactors=FALSE)
