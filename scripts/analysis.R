################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### analysis.R: generate metrics ###############################
rm(list = ls())
library(plyr)
library(lubridate)

load("processed_data/LEAPOA_ind.Rdata")

stat <- ddply(LEAPOA_ind, .(Joint), summarize,
              BLto3m = paste0(sum(complete.3m==1, na.rm=TRUE), " (",sum(complete.3m==1 & SF==1, na.rm=TRUE), " SF)"),
              complete.BLto1y = paste0(sum(complete_set==1, na.rm=TRUE), " (", sum(complete_set==1 & SF==1, na.rm=TRUE), " SF)"),
              BLto1y.ex6m = paste0(sum(complete.ex6mdata==1, na.rm=TRUE), " (", sum(complete.ex6mdata==1 & SF==1, na.rm=TRUE), " SF)"),
              BL1y.pl2 = paste0(sum(BL1Y.pl2==1, na.rm=TRUE), " (", sum(BL1Y.pl2==1 & SF==1, na.rm=TRUE), " SF)"),
              BL1y.pl1 = paste0(sum(BL1Y.pl1==1, na.rm=TRUE), " (", sum(BL1Y.pl1==1 & SF==1, na.rm=TRUE), " SF)")
              
#               BLto3m.SF = sum(complete.3m==1 & SF==1, na.rm=TRUE),
#               complete.BLto1y.SF = sum(complete_set==1 & SF==1, na.rm=TRUE),
#               BLto1y.ex6m.SF = sum(complete.ex6mdata==1 & SF==1, na.rm=TRUE),
#               BL1y.pl2.SF = sum(BL1Y.pl2==1, na.rm=TRUE),
#               BL1y.pl1.SF = sum(BL1Y.pl1==1 & SF==1, na.rm=TRUE)
)

stat.t <- data.frame(t(stat[,2:length(stat)]))
names(stat.t) <- stat[,1]

write.csv(stat.t, file="output/stats.csv")
