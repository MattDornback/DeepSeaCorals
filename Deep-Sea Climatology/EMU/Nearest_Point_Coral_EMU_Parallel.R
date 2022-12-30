library(dplyr)
library(readr)
library(rgeos)
library(parallel)

#set working directory
setwd('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU')
#file path to corals data and EMU data
Coral_path <- "C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/DSCRTP_NatDB_20190709-3/DSCRTP_NatDB_20190709-3.csv"
EMU_path <- "C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Pacific_EMU_Bottom.csv"

#read in the emu data
EMU <- read_csv(EMU_path)

# read in coral data
Coral <- read_csv(Coral_path, na = c("NA", -999))

# First, send an instance of R to each core on the local machine
# The detectCores() function detects the number of phyiscal cores and sends R to 
# all of them, but one could replace the function with a number to utilize fewer 
# than the maximum number of cores
cl=makeCluster(detectCores())


# Load package on all instances of R on all cores
clusterEvalQ(cl,c(library(rgeos)))

#filter rows and select columns to slim down the data being processed, rename the lat/lon in the EMU
EMU_c <- EMU %>% filter(OceanName == c("South Pacific")) %>% 
  select(OBJECTID, POINT_X, POINT_Y) %>% rename(Longitude = POINT_X, Latitude = POINT_Y)

# promote the input lists to SpatialPointsDataFrames
coordinates(EMU_c) <- c("Longitude", "Latitude") 

# rename all EMU columns with a prefix "EMU_" so I can tell them from the DSC columns after joining them
colnames(EMU) <- paste("EMU", colnames(EMU), sep = "_")

# Create an empty tibble data.frame to hold the output of the for loop
Coral_EMU <- tibble()

#Want to check how long things take
#create oject tstart by getting current time with Sys.time() function.
tstart <- Sys.time()
#show tstart in console
tstart

#read in the corals in 1000 row chunks
for(i in 1:700){
  start_slice <- 1000 * (i - 1) + 1
  end_slice <- start_slice + 1000

  #filter rows and select columns to slim down the data being processed
  Coral_c <- Coral %>% filter(Ocean == c("South Pacific")) %>% select(CatalogNumber, Longitude, Latitude) %>% slice(start_slice:end_slice)

  # promote the input lists to SpatialPointsDataFrames
  coordinates(Coral_c) <- c("Longitude", "Latitude")

  # iterate by rows with apply, 1,
  # find the distances between EMU and Coral points with gDistance
  # find the shortest distance with which.min
  # create a new column in the Coral dataframe "nearest_EMU" that returns the index value of the EMU
  Coral_c$nearest_EMU_index <- parApply(cl,gDistance(EMU_c, Coral_c, byid=TRUE), 1, which.min)

  # iterate through the rows of Coral
  # return the the EMU OBJECTID of each EMU index value
  Coral_c$EMU_OBJECTID <- sapply(Coral_c$nearest_EMU_index, function(x) EMU_c[x,]$OBJECTID)

  # convert Coral_c (spatialpointdataframe) to a normal dataframe and grab just the CatalogNumber and EMU_OBJECTID columns
  Coral_c_df <- as.data.frame(Coral_c) %>% select(CatalogNumber, EMU_OBJECTID)

  # remove Coral_c to clean up and stuff
  rm(Coral_c)

  # join Coral data with EMU OBJECTID using CatalogNumber as the key
  Coral_OID <- right_join(Coral, Coral_c_df, by = "CatalogNumber")

  # remove Coral to clean up and stuff
  #rm(Coral)
  rm(Coral_c_df)

  # join Coral data with EMU data using OBJECTID
  Coral_EMU_Chunk <- left_join(Coral_OID, EMU, by = "EMU_OBJECTID")

  # remove Coral w/ ObjectID to clean up and stuff
  rm(Coral_OID)

  # bind the rows of the Coral_EMU_Chunk to the Coral_EMU dataframe that is outside the for loop
  Coral_EMU <- bind_rows(Coral_EMU, Coral_EMU_Chunk)
  
  # remove Coral_EMU_Chunk to clean up and stuff
  rm(Coral_EMU_Chunk)
  
#end the chunking for loop
}

tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed

# save to and r rds file
write_rds(Coral_EMU, "Coral_EMU_20190709-3_South.rds")
