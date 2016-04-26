################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### merge.R: merge data ###############################

### combine timepoints for plasma samples
TKR_PL <- rbind.fill(TKR_PL_BL, TKR_PL_6W, TKR_PL_3M, TKR_PL_6M, TKR_PL_1Y)
TKR_PL_Q <- rbind(TKR_PL_Q_BL, TKR_PL_Q_6W, TKR_PL_Q_3M, TKR_PL_Q_6M, TKR_PL_Q_1Y)

THR_PL <- rbind.fill(THR_PL_BL, THR_PL_6W, THR_PL_3M, THR_PL_6M, THR_PL_1Y)
THR_PL_Q <- rbind(THR_PL_Q_BL, THR_PL_Q_6W, THR_PL_Q_3M, THR_PL_Q_6M, THR_PL_Q_1Y)

### remove duplicate/parent rows
TKR_PL <- subset(TKR_PL, TKR_PL$ReceivedEventParameters...Received.Quality != "" & TKR_PL$StorageContainer...Name != "")
THR_PL <- subset(THR_PL, THR_PL$ReceivedEventParameters...Received.Quality != "" & THR_PL$StorageContainer...Name != "")

TKR_SF <- subset(TKR_SF, TKR_SF$StorageContainer...Name != "")
THR_SF <- subset(THR_SF, THR_SF$StorageContainer...Name != "")

### add timepoint for SDF
SDF_PL$CollectionProtocolEvent...Collection.Point.Label <- ifelse(grepl("^Pre",SDF_PL$Specimen...Label), "Pre-Surgery",
                                                  ifelse(grepl("^6wk", SDF_PL$Specimen...Label), "6 week Follow-up",
                                                         ifelse(grepl("^3m", SDF_PL$Specimen...Label), "3 month Follow-up",
                                                                ifelse(grepl("^6m", SDF_PL$Specimen...Label), "6 month Follow-up",
                                                                       ifelse(grepl("^1y", SDF_PL$Specimen...Label), "1 year Follow-up", NA)))))

# add missing timepoints
SDF_PL$CollectionProtocolEvent...Collection.Point.Label[grep("^164_SDF_Pl", SDF_PL$Specimen...Label)] <- "Pre-Surgery"
SDF_PL$CollectionProtocolEvent...Collection.Point.Label[grep("^57_SDF_Pl", SDF_PL$Specimen...Label)] <- "Pre-Surgery"


### merge SAP quality/quantity and CaTissue reports together
TKR_PL <- merge(TKR_PL, TKR_PL_Q[,c("Specimen.Label", "Quality")], by.x="Specimen...Label", by.y="Specimen.Label", all.x=T, all.y=F)
THR_PL <- merge(THR_PL, THR_PL_Q[,c("Specimen.Label", "Quality")], by.x="Specimen...Label", by.y="Specimen.Label", all.x=T, all.y=F)
SDF_PL <- merge(SDF_PL, SDF_PL_Q[,c("Specimen.Label", "Quality")], by.x="Specimen...Label", by.y="Specimen.Label", all.x=T, all.y=F)

TKR_SF <- merge(TKR_SF, TKR_SF_QUANTITY[,c("Specimen.Label", "Initial.Quantity")], by.x="Specimen...Label", by.y="Specimen.Label", all.x=T, all.y=F)
THR_SF <- merge(THR_SF, THR_SF_QUANTITY[,c("Specimen.Label", "Initial.Quantity")], by.x="Specimen...Label", by.y="Specimen.Label", all.x=T, all.y=F)

### combine plasma and SF datasets
PL_data <- rbind.fill(TKR_PL, THR_PL, SDF_PL)
SF_data <- rbind.fill(TKR_SF, THR_SF)

### extract variables from plasma datasets

PL <- data.frame(MRN = PL_data$ParticipantMedicalIdentifier...Medical.Record.Number,
                 Clinical.Status = PL_data$SpecimenCollectionGroup...Clinical.Status,
                 Procedure = PL_data$CollectionEventParameters...Collection.Procedure,
                 Joint = ifelse(grepl("Knee", PL_data$CollectionEventParameters...Collection.Procedure), "Knee",
                                      ifelse(grepl("Hip", PL_data$CollectionEventParameters...Collection.Procedure), "Hip", "Spine")),
                 Timepoint = PL_data$CollectionProtocolEvent...Collection.Point.Label,
                 Collection.Date = mdy(do.call(rbind,strsplit(PL_data$CollectionEventParameters...Timestamp, " "))[,1]),
                 Specimen.Label = PL_data$Specimen...Label,
                 Quality = PL_data$Quality,
                 Available = PL_data$Specimen...Available.Quantity,
                 Location = paste(PL_data$StorageContainer...Name, PL_data$SpecimenPosition...Position.Dimension.One, sep=", "),
                 Comment = PL_data$Specimen...Comment,
                 ThawDate = PL_data$ThawEventParameters...Timestamp,
                 ThawComment = PL_data$ThawEventParameters...Comment,
                 Biohazard = PL_data$Biohazard...Name,
                 stringsAsFactors=FALSE
)


### extract variables from SF datasets

SF <- data.frame(MRN = SF_data$ParticipantMedicalIdentifier...Medical.Record.Number,
                 Procedure = SF_data$CollectionEventParameters...Collection.Procedure,
                 Joint = ifelse(grepl("Knee", SF_data$CollectionEventParameters...Collection.Procedure), "Knee", "Hip"),
                 Collection.Date = mdy(do.call(rbind,strsplit(SF_data$CollectionEventParameters...Timestamp, " "))[,1]),
                 Specimen.Label = SF_data$Specimen...Label,
                 Initial = SF_data$Initial.Quantity,
                 Available = SF_data$Specimen...Available.Quantity,
                 Location = paste(SF_data$StorageContainer...Name, SF_data$SpecimenPosition...Position.Dimension.One, sep=", "),
                 Comment = SF_data$Specimen...Comment,
                 ThawDate = SF_data$ThawEventParameters...Timestamp,
                 ThawComment = SF_data$ThawEventParameters...Comment,
                 Thaw = ifelse(SF_data$Initial.Quantity != SF_data$Specimen...Available.Quantity, 1, 0),
                 Biohazard = SF_data$Biohazard...Name,
                 stringsAsFactors=FALSE
                 )
write.csv(SF, file="processed_data/SF.csv", row.names=F, na="")

### get study IDs
split_PL_IDs <- do.call(rbind, strsplit(PL$Specimen.Label, "[_]"))
PL_IDs <- apply(split_PL_IDs, 1, function(x) {
  ifelse(grepl("(THR|TKR|SDF)", x[2]), x[1], x[2])
})
PL_IDs[PL_IDs=="563H"] <- 563
PL_IDs[PL_IDs=="168-SP"] <- 168

split_SF_IDs <- do.call(rbind,strsplit(SF$Specimen.Label, "[_]"))
SF_IDs <- apply(split_SF_IDs, 1, function(x) {
  ifelse(grepl("Op", x[1]), x[2], x[1])
})
SF_IDs[SF_IDs=="467K"] <- 467

### add study IDs to datasets
PL <- cbind(ID=PL_IDs, PL)
SF <- cbind(ID=SF_IDs, SF)

### clean blanks
PL[PL==""] <- NA
SF[SF==""] <- NA

## save data
save(PL, file="processed_data/PL.Rdata")
save(SF, file="processed_data/SF.Rdata")
write.csv(PL, file="processed_data/PL.csv", row.names=FALSE, na="")
write.csv(SF, file="processed_data/SF.csv", row.names=FALSE, na="")