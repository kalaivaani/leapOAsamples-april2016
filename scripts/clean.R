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


##################### synovial fluid summary ########################
# create protease inhibitor indicator
SF$ProtInhib <- ifelse(grepl("roteas", SF$Comment), 1, 0)

# summary data
SF_ind <- ddply(SF, .(ID, Joint), summarize,
                NAliquot = sum(Available>0),
                NInhib = sum(ProtInhib==1),
                Volumes = paste(paste(table(Available), paste0(names(table(Available)), "mL"), sep=": "), collapse="; "),
                Biohazard = head(Biohazard, 1))

##################### plasma summary ###########################
# create protease inhibitor indicator
PL$ProtInhib <- ifelse(grepl("roteas", PL$Comment), 1, 0)

# create hemolyzed indicator
PL$Hemolyzed <- ifelse(PL$Quality=="Hemolyzed" | grepl("emoly", PL$Comment), 1, 0)

# add fasting status from comment
PL$Clinical.Status[grepl("on[ |\\-]fasting", PL$Comment)] <- "Non-fasting"
PL$Clinical.Status[grepl("[^non][ |\\-]fasting", PL$Comment)] <- "Fasting"

# exclude hemolyzed, thawed
PL_excl <- subset(PL, PL$Hemolyzed != 1 & is.na(PL$ThawDate))

PL_ind <- ddply(PL_excl, .(ID, Joint, Timepoint), summarize,
                NAliquot = sum(Available>0),
                NInhib = sum(ProtInhib==1),
                Fasting = sum(Clinical.Status=="Fasting"),
                Volumes = paste(paste(table(Available), paste0(names(table(Available)), "mL"), sep=": "), collapse="; "),
                Biohazard = head(Biohazard, 1))
