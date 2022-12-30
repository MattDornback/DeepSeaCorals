#Script to create abstracts for Datasets based on DatasetID, information in the database, and the associated methods url.


library(dplyr)

#set my working directory
setwd('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Datasets/Abstracts/')

#Load index of DatasetIDs and associated methods
Dataset_Key <- read.csv("20181009_DatasetID_Key.csv",stringsAsFactors = F)

#Load DSCRTP Database
RawDatabase <- read.csv("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/DSCRTP_NatDB_20181005-0/DSCRTP_NatDB_20181005-0.csv", header = T, na.strings = "-999")

#Filter Database for Flagged records
Database <- RawDatabase %>%
  filter(Flag == "0")

#Iterate over Dataset_Key rows, and subset Database for each DatasetID.
#Taking the DatasetID subset extract information to paste into the abstract
#Abstracts are different depending on type of dataset (e.g. Repository)
for(i in 1:nrow(Dataset_Key)) {
  DSID <- Dataset_Key[i,]
    This_Dataset <- filter(Database, DatasetID == DSID[,1])
    #print(head(This_Dataset$DatasetID, 1))
    print(DSID[,1])
    if (DSID[,3] == "Repository") {
      Dataset_Key[i,6] <- rbind(paste("This subset of the Deep Sea Coral Research and Technology Program's National Database. The data was acquired for the National Database from the repository ", DSID[,4], " and was provided by ", paste(unique(This_Dataset$DataContact), collapse = ", "), ". Changes to the original dataset may have been made to conform to the National Database Schema and for quality control purposes. This is a collection of ", paste(unique(This_Dataset$DataProvider), collapse = ", "), " records collected with ", paste(unique(This_Dataset$SamplingEquipment), collapse = ", "), " conducted from ", min(This_Dataset$ObservationYear), "-", max(This_Dataset$ObservationYear), " in the ", paste(unique(This_Dataset$LargeMarineEcosystem), collapse = ", "), ". Further information can be found at ", DSID[,5], sep = ""))
      print(min(This_Dataset$ObservationYear))
      print(max(This_Dataset$ObservationYear))
    } else if (DSID[,3] == "Literature"){
      Dataset_Key[i,6] <- rbind(paste("This subset of the Deep Sea Coral Research and Technology Program's National Database. The data was acquired for the National Database from the literature ", DSID[,4], " and was provided by ", paste(unique(This_Dataset$DataContact), collapse = ", "), ". Changes to the original dataset may have been made to conform to the National Database Schema and for quality control purposes. This is a collection of ", paste(unique(This_Dataset$DataProvider), collapse = ", "), " records collected with ", paste(unique(This_Dataset$SamplingEquipment), collapse = ", "), " conducted from ", min(This_Dataset$ObservationYear), "-", max(This_Dataset$ObservationYear), " in the ", paste(unique(This_Dataset$LargeMarineEcosystem), collapse = ", "), ". Further information can be found at ", DSID[,5], sep = ""))
    } else if (DSID[,3] == "Program"){
      Dataset_Key[i,6] <- rbind(paste("This subset of the Deep Sea Coral Research and Technology Program's National Database. The data was acquired for the National Database from the sampling program ", DSID[,4], " and was provided by ", paste(unique(This_Dataset$DataContact), collapse = ", "), ". Changes to the original dataset may have been made to conform to the National Database Schema and for quality control purposes. This is a collection of ", paste(unique(This_Dataset$DataProvider), collapse = ", "), " records collected with ", paste(unique(This_Dataset$SamplingEquipment), collapse = ", "), " conducted from ", min(This_Dataset$ObservationYear), "-", max(This_Dataset$ObservationYear), " in the ", paste(unique(This_Dataset$LargeMarineEcosystem), collapse = ", "), ". Further information can be found at ", DSID[,5], sep = ""))
    } else if (DSID[,3] == "Cruise"){
      Dataset_Key[i,6] <- rbind(paste("This subset of the Deep Sea Coral Research and Technology Program's National Database. The data was acquired for the National Database from the research cruise ", DSID[,4], " and was provided by ", paste(unique(This_Dataset$DataContact), collapse = ", "), ". Changes to the original dataset may have been made to conform to the National Database Schema and for quality control purposes. This is a collection of ", paste(unique(This_Dataset$DataProvider), collapse = ", "), " records collected by ", paste(unique(This_Dataset$SamplingEquipment), collapse = ", "), " conducted from ", min(This_Dataset$ObservationYear), "-", max(This_Dataset$ObservationYear), " in the ", paste(unique(This_Dataset$LargeMarineEcosystem), collapse = ", "), ". Further information can be found at ", DSID[,5], sep = ""))
    } else {
      print("Fail")
    }
    print(Dataset_Key[i,6])
    rm(This_Dataset)
}

write.csv(Dataset_Key,"Dataset_Key_Output_1.csv",row.names=F)
  
