################################### Project: determine sample sizes for LEAP-OA analysis #####################################
# hip, knee, and spine cohorts
# survey data and plasma + synovial fluid (hip and knee)
# date: 2016-04-26
# written by: Kala Sundararajan
# LEAP-OA master list, survey indicators from DADOS database (2016-04-26)
# biosample data from CaTissue/SAP (pulled by Kim Perry)


########################### merge.R: merge data ###############################

### combine plasma datasets
PL <- rbind.fill(rbind(TKR_PL_BL, TKR_PL_6W, TKR_PL_3M6M1Y), 
                      rbind(THR_PL_BL, THR_PL_6W, THR_PL_3M6M1Y), 
                      SDF_PL)
# fix date collected
PL$Collection.Date <- ymd(do.call(rbind,strsplit(PL$Collection.Date, " "))[,1])

# create thaw indicator
PL$Thaw <- ifelse(PL$Available.Quantity < PL$Initial.Quantity | 
                         grepl("deplet", PL$Specimen.Comments) |
                         grepl("REB", PL$Specimen.Comments), 
                               1, 0)

# create protease inhibitor indicator
PL$Protease <- ifelse(grepl("roteas", PL$Specimen.Comments), 1, 0)

# create hemolyzed indicator
PL$Hemolyzed <- ifelse(PL$Quality=="Hemolyzed" | grepl("emoly", PL$Specimen.Comments), 1, 0)

# add fasting status from comment
PL$Clinical.Status[grepl("[^on] [f|F]asting", PL$Specimen.Comments) | grepl("^[f|F]asting", PL$Specimen.Comments)] <- "Fasting"
PL$Clinical.Status[grepl("on[ |-][f|F]asting", PL$Specimen.Comments)] <- "Non-fasting"


#### create SF data indicators

# date collected
SF$Collection.Date <- ymd(do.call(rbind,strsplit(SF$Collection.Date, " "))[,1])

# thaw indicator
SF$Thaw <- ifelse(SF$Available.Quantity < SF$Initial.Quantity | 
                    grepl("deplet", SF$Specimen.Comments) |
                    grepl("REB", SF$Specimen.Comments), 
                  1, 0)

# protease inhibitor indicator
SF$Protease <- ifelse(grepl("roteas", SF$Specimen.Comments), 1, 0)

###### get study IDs ########
# PL$ID <- sub("(Study ID |Leap ID )", "", PL$Event.Comments)
# PL$ID <- sub("-", "", PL$ID)
# PL$ID[grep("(artilage|RCT)", PL$ID)] <- NA
# PL$ID <- sub("^[ *|\n*]", "", PL$ID)
# PL$ID <- sub("[ *|\n*]$", "", PL$ID)
# PL$ID[PL$ID=="168SP"] <- 168
# 
# 
# SF$ID <- sub("(Study ID |Leap ID )", "", SF$Event.Comments)
# SF$ID <- sub("-", "", SF$ID)
# SF$ID[SF$Specimen.Label=="462_TKR_SF_25"] <- 462
# SF$ID[SF$ID=="464K"] <- 464


split_PL_IDs <- do.call(rbind, strsplit(PL$Specimen.Label, "[_]"))
PL_IDs <- apply(split_PL_IDs, 1, function(x) {
  ifelse(grepl("(THR|TKR|SDF)", x[2]), x[1], x[2])
})
PL_IDs <- sub("^ *", "", PL_IDs)
PL_IDs <- sub(" *$", "", PL_IDs)
PL_IDs[PL_IDs=="563H"] <- 563
PL_IDs[PL_IDs=="168-SP"] <- 168

split_SF_IDs <- do.call(rbind,strsplit(SF$Specimen.Label, "[_]"))
SF_IDs <- apply(split_SF_IDs, 1, function(x) {
  ifelse(grepl("Op", x[1]), x[2], x[1])
})
SF_IDs <- sub("^ *", "", SF_IDs)
SF_IDs <- sub(" *$", "", SF_IDs)
SF_IDs[SF_IDs=="467K"] <- 467

### add study IDs to datasets
PL <- cbind(ID=PL_IDs, PL)
SF <- cbind(ID=SF_IDs, SF)

### clean blanks
PL[PL==""] <- NA
SF[SF==""] <- NA

# rename MRN variable
names(PL)[names(PL)=="Medical.Record.Number"] <- names(SF)[names(SF)=="Medical.Record.Number"] <- "MRN"

# add Joint variable
PL$Joint <- ifelse(grepl("Hip", PL$Procedure), "Hip",
                   ifelse(grepl("Knee", PL$Procedure), "Knee", "Spine"))
SF$Joint <- ifelse(grepl("Hip", SF$Procedure), "Hip",
                   ifelse(grepl("Knee", SF$Procedure), "Knee", "Spine"))


###################### check MRNs ########################
# plasma
PlasmaMRNcheck <- ddply(PL, .(ID, Joint), summarize,
                        nMRNs = length(levels(as.factor(MRN))),
                        MRNs = paste(levels(as.factor(MRN)), collapse="; "))
#write.csv(subset(PlasmaMRNcheck, PlasmaMRNcheck$nMRNs>1), file="processed_data/mislabelledPlasma2.csv", row.names=F, na="")

# SF
SFMRNcheck <- ddply(SF, .(ID, Joint), summarize,
                    nMRNs = length(levels(as.factor(MRN))),
                    MRNs = paste(levels(as.factor(MRN)), collapse="; "))
#write.csv(subset(SFMRNcheck, SFMRNcheck$nMRNs>1), file="processed_data/mislabelledSF2.csv", row.names=F, na="")

################# save data #########################
save(PL, file="processed_data/PL.Rdata")
save(SF, file="processed_data/SF.Rdata")
write.csv(PL, file="processed_data/PL.csv", row.names=FALSE, na="")
write.csv(SF, file="processed_data/SF.csv", row.names=FALSE, na="")