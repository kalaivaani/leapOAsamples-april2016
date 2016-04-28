################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### clean.R: clean data ###############################

######################## survey summary #############################
OA_Hip$Joint <- "Hip"
OA_Knee$Joint <- "Knee"
OA_Spine$Joint <- "Spine"

OAData <- list(Hip=OA_Hip, Knee=OA_Knee, Spine=OA_Spine)

OA_indicators <- lapply(OAData, function(dat) {
  BL <- apply(dat[,grep("BASELINE", names(dat))], 1, function(responses) {
    ifelse(sum(!is.na(responses))>=3, 1, 0)
  })
  x6w <- apply(dat[,grep("6WEEK", names(dat))], 1, function(responses) {
    ifelse(sum(!is.na(responses))>=3, 1, 0)
  })
  x3m <- apply(dat[,grep("3MONTH", names(dat))], 1, function(responses) {
    ifelse(sum(!is.na(responses))>=3, 1, 0)
  })
  x6m <- apply(dat[,grep("6MONTH", names(dat))], 1, function(responses) {
    ifelse(sum(!is.na(responses))>=3, 1, 0)
  })
  x1y <- apply(dat[,grep("1YEAR", names(dat))], 1, function(responses) {
    ifelse(sum(!is.na(responses))>=3, 1, 0)
  })
  return(data.frame(ID=dat$SUBJECT_ID, Joint=dat$Joint, DOS=dat$DOE, BL, x6w, x3m, x6m, x1y, stringsAsFactors=FALSE))
})

OA_indicators <- do.call(rbind, OA_indicators)
OA_indicators$followups <- apply(OA_indicators[,5:8], 1, sum)
#View(subset(OA_indicators, OA_indicators$x1y==1 & OA_indicators$followups >= 2))

### merge with status from master list
# create Joint column in master list
LEAPOA_master$Joint <- ifelse(grepl("KNEE", LEAPOA_master$Study.Name), "Knee",
                              ifelse(grepl("HIP", LEAPOA_master$Study.Name), "Hip", "Spine"))

OA_indicators <- merge(LEAPOA_master[,c("Subject.ID", "Joint", "Status")], OA_indicators, 
                       by.x=c("Subject.ID", "Joint"), 
                       by.y=c("ID", "Joint"), 
                       all.x=TRUE, all.y=FALSE)

OA_indicators$DOS <- ymd(OA_indicators$DOS)

# restrict to ongoing or withdrawn patients
OA_indicators <- subset(OA_indicators, OA_indicators$Status %in% c("ONGOING", "WITHDRAWN"))

# remove rows with no survey data / no followups
OA_indicators <- subset(OA_indicators, !is.na(OA_indicators$BL) & OA_indicators$followups != 0)

##### check biobank/DADOS MRNs ###########
PL$ID <- as.character(PL$ID)
PlasmaMRNs <- merge(LEAPOA_master[,c("Subject.ID","Joint","MRN")], PL[,c("ID", "Joint", "MRN", "Collection.Point", "Specimen.Label")],
              by.x=c("Subject.ID", "Joint"), by.y=c("ID", "Joint"), all.x=F, all.y=F)
names(PlasmaMRNs) <- c("ID", "Joint", "DADOS.MRN", "CaTissue.MRN", "Collection.Point", "Label")
  
SF$ID <- as.character(SF$ID)
SFMRNs <- merge(LEAPOA_master[,c("Subject.ID","Joint","MRN")], SF[,c("ID", "Joint", "MRN", "Specimen.Label")],
                    by.x=c("Subject.ID", "Joint"), by.y=c("ID", "Joint"), all.x=F, all.y=F)
names(SFMRNs) <- c("ID", "Joint", "DADOS.MRN", "CaTissue.MRN", "Label")


mismatchMRN <- rbind.fill(subset(PlasmaMRNs, PlasmaMRNs$DADOS.MRN != PlasmaMRNs$CaTissue.MRN),
                          subset(SFMRNs, SFMRNs$DADOS.MRN != SFMRNs$CaTissue.MRN))
write.csv(mismatchMRN, file="processed_data/mismatchMRNs3.csv", row.names=F, na="")

############ fix mislabelled plasma samples ##############
# 105 spine: 3097163 --> 112 spine
PL$ID[PL$ID==105 & PL$Joint=="Spine" & PL$MRN == 3097163] <- 112

# 188 spine: 3366137 --> no ID
PL$ID[PL$ID==188 & PL$Joint=="Spine" & PL$MRN == 3366137] <- NA

# 56 spine: 4204012 --> no ID
PL$ID[PL$ID==56 & PL$Joint=="Spine" & PL$MRN == 4204012] <- NA

# 563 knee: 2327357 --> change joint to 'Hip'
PL$Joint[PL$ID==563 & PL$Joint=="Knee" & PL$MRN==2327357] <- "Hip"

# 57 spine: 2793324 --> no ID
PL$ID[PL$ID==57 & PL$Joint=="Spine" & PL$MRN == 2793324] <- NA

# 59 spine: 4034872 --> no ID
PL$ID[PL$ID==59 & PL$Joint=="Spine" & PL$MRN == 4034872] <- NA

# 97 hip: 3547478 --> change joint to 'Spine'
PL$Joint[PL$ID==97 & PL$Joint=="Hip" & PL$MRN==3547478] <- "Spine"

# 244 knee: 1276485 --> 244-hip
PL$Joint[PL$ID==244 & PL$Joint=="Knee" & PL$MRN==1276485] <- "Hip"

# 388 knee: 167915 --> 388-hip
PL$Joint[PL$ID==388 & PL$Joint=="Knee" & PL$MRN==167915] <- "Hip"

# 41 spine: 4217918 --> not leap-OA
PL$ID[PL$ID==41 & PL$Joint=="Spine" & PL$MRN==4217918] <- NA

# 53 spine: 4064458 --> not leap-OA
PL$ID[PL$ID==53 & PL$Joint=="Spine" & PL$MRN==4064458] <- NA

# 32 spine: 3986886 --> should be 35 spine
PL$ID[PL$ID == 32 & PL$Joint=="Spine" & PL$MRN==3986886] <- 35

# 720 knee: 3028808 --> should be hip
PL$Joint[PL$ID==720 & PL$MRN==3028808 & PL$Joint=="Knee"] <- "Hip"

### SF
# 355 knee: 572647 --> should be 335-knee
SF$ID[SF$ID==355 & SF$Joint=="Knee" & SF$MRN==572647] <- 335

# 278 hip: label Op_278_THR_SF_79 --> should be B149
SF$ID[SF$Specimen.Label=="Op_278_THR_SF_79"] <- "B149"

# 314 hip: label PreOp_314_THR_SF_232 (?) --> should be B136
SF$ID[SF$Specimen.Label=="PreOp_314_THR_SF_232"] <- "B136"


##################### synovial fluid summary ########################

# summary data
SF_ind <- ddply(SF, .(ID, Joint), summarize,
                SF.Date = head(Collection.Date,1),
                SF.Aliquot = sum(Available.Quantity>0 & Thaw==0),
                SF.Clean = sum(Protease==0 & Thaw==0),
                SF.Volumes = paste(paste(table(Available.Quantity), paste0(names(table(Available.Quantity)), "mL"), sep=": "), collapse="; "),
                SF.Biohazard = head(Biohazard, 1))

##################### plasma summary ###########################


PL_ind <- ddply(PL, .(ID, Joint, Collection.Point), summarize,
                Date = head(Collection.Date,1),
                Aliquot = sum(Available.Quantity>0 & Hemolyzed==0 & Thaw==0),
                Clean = sum(Protease==0 & Hemolyzed==0 & Thaw==0),
                Hemolyzed=sum(Hemolyzed==1),
                Thawed=sum(Thaw==1),
                Status = head(names(table(Clinical.Status))[which.max(table(Clinical.Status))]),
                Volumes = paste(paste(table(Available.Quantity), paste0(names(table(Available.Quantity)), "mL"), sep=": "), collapse="; "),
                Biohazard = head(Biohazard, 1))

# change timepoint labels
PL_ind$Collection.Point[PL_ind$Collection.Point=="Pre-Surgery"] <- 1
PL_ind$Collection.Point[PL_ind$Collection.Point=="6 week Follow-up"] <- 2
PL_ind$Collection.Point[PL_ind$Collection.Point=="3 month Follow-up"] <- 3
PL_ind$Collection.Point[PL_ind$Collection.Point=="6 month Follow-up"] <- 4
PL_ind$Collection.Point[PL_ind$Collection.Point=="1 year Follow-up"] <- 5

PL_ind <- PL_ind[order(PL_ind$Collection.Point),]

# reshape wide
PL_ind_wide <- reshape(PL_ind, timevar="Collection.Point", idvar=c("ID", "Joint"), sep=".", direction="wide")



####### merge together survey and biobank data ############

bio_ind <- merge(SF_ind, PL_ind_wide, by=c("ID", "Joint"), all.x=T, all.y=T)

LEAPOA_ind <- merge(OA_indicators, bio_ind, by.x=c("Subject.ID", "Joint"), by.y=c("ID", "Joint"), all.x=TRUE, all.y=FALSE)

# create indicators
LEAPOA_ind$SF <- ifelse(LEAPOA_ind$SF.Aliquot>1, 1, 0)

LEAPOA_ind$complete.1 <- ifelse(LEAPOA_ind$BL==1 & LEAPOA_ind$Aliquot.1>1, 1, 0)
LEAPOA_ind$complete.2 <- ifelse(LEAPOA_ind$x6w==1 & LEAPOA_ind$Aliquot.2>1, 1, 0)
LEAPOA_ind$complete.3 <- ifelse(LEAPOA_ind$x3m==1 & LEAPOA_ind$Aliquot.3>1, 1, 0)
LEAPOA_ind$complete.4 <- ifelse(LEAPOA_ind$x6m==1 & LEAPOA_ind$Aliquot.4>1, 1, 0)
LEAPOA_ind$complete.5 <- ifelse(LEAPOA_ind$x1y==1 & LEAPOA_ind$Aliquot.5>1, 1, 0)

LEAPOA_ind$complete_set <- apply(LEAPOA_ind[,c(grep("complete\\.[0-5]", names(LEAPOA_ind)))], 1, function(x) ifelse(sum(x, na.rm=T)==5, 1, 0))

LEAPOA_ind$complete.ex6mPl <- ifelse(LEAPOA_ind$complete.1 == 1
                                         & LEAPOA_ind$complete.2 == 1
                                         & LEAPOA_ind$complete.3 == 1
                                         & LEAPOA_ind$x6m == 1
                                         & LEAPOA_ind$complete.5 == 1,
                                         1, 0)
LEAPOA_ind$complete.ex6mdata <- ifelse(LEAPOA_ind$complete.1 == 1
                                            & LEAPOA_ind$complete.2 == 1
                                            & LEAPOA_ind$complete.3 == 1
                                            & LEAPOA_ind$complete.5 == 1,
                                          1, 0)

LEAPOA_ind$complete.3m <- apply(LEAPOA_ind[,c(grep("complete\\.[0-3]", names(LEAPOA_ind)))], 1, function(x) ifelse(sum(x, na.rm=T)==3, 1, 0))

LEAPOA_ind$BL1Y.pl2 <- apply(LEAPOA_ind[,c(grep("complete\\.[0-5]", names(LEAPOA_ind)))], 1, function(x) ifelse(x[1]==1 & x[5]==1 & sum(x, na.rm=TRUE)>=4, 1, 0))
LEAPOA_ind$BL1Y.pl1 <- apply(LEAPOA_ind[,c(grep("complete\\.[0-5]", names(LEAPOA_ind)))], 1, function(x) ifelse(x[1]==1 & x[5]==1 & sum(x, na.rm=TRUE)>=3, 1, 0))

LEAPOA_ind <- LEAPOA_ind[order(LEAPOA_ind$Joint,LEAPOA_ind$Subject.ID),]

write.csv(LEAPOA_ind, file="processed_data/LEAPOAsamples.csv", row.names=F, na="")
save(LEAPOA_ind, file="processed_data/LEAPOA_ind.Rdata")
