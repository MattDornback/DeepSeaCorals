# Latest version: Assign closest points from a second point set

library(sp)
library(dplyr)
library(readr)

#set working directory
setwd('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology')
#file path to corals data and EMU data
Coral_path <- "C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/DSCRTP_NatDB_20190418-0_Science_Fields/DSCRTP_NatDB_20190418-0_Science_Fields.csv"
EMU_path <- "C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU_Bottom.csv"

#read in the corals and emu data
Coral <- read_csv(Coral_path, na = c("NA", -999))
EMU <- read_csv(EMU_path)

#filter rows and select columns to slim down the data being processed, rename the lat/lon in the EMU
Coral <- Coral %>% select(CatalogNumber, Longitude, Latitude)
EMU <- EMU %>% select(OBJECTID, POINT_X, POINT_Y) %>% rename(Longitude = POINT_X, Latitude = POINT_Y)

# promote the input lists to SpatialPointsDataFrames
coordinates(Coral) <- c("Longitude", "Latitude")
coordinates(EMU) <- c("Longitude", "Latitude")             

# Remove temparature points with 'invalid' value of -199
#validTempPoints <- temperaturePoints[!temperaturePoints$temperature %in% c(-199),]

#  Define these vectors, used in the loop.
closestSiteVec <- vector(mode = "numeric",length = nrow(Coral))
minDistVec     <- vector(mode = "numeric",length = nrow(Coral))

# Get the vector index of the temperature station closest to each field station.
# Use the spDistsN1 function to compute the distance vector between each
# field station site and all of the temperature stations. Then, find and
# retain the actual temperature, and the index of the closest temperature
# to each transect station.
#
# spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
#
# where:
#         pointList   : List of candidate points.
#         pointToMatch: Single point for which we seek the closest point in pointList.
#         longlat     : TRUE  computes Great Circle distance in km,
#                       FALSE computes Euclidean distance in units of input geographic coordinates
#
# We use Great Circle distance to increase distance calculation accuracy at high latitudes
# See the discussion of distance units in the header portion of this file
#
# minDistVec stores distance from the closest temperature station to each density measurement point.
# closestSiteVec stores the index of the closest temperature station to each density measurement point.
#
#Want to check how long things take
#create oject tstart by getting current time with Sys.time() function.
tstart <- Sys.time()
#show tstart in console
tstart

for (i in 1 : nrow(Coral)) {
  distVec <- spDistsN1(EMU, Coral[i,],longlat = TRUE)
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}

tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed
#
# Create the Temperature Assignment table: merge the temperature point list with the transect point list
# into a five-column table by merging the temperature point and transect point lists.
#
PointAssignEMU <- as(EMU[closestSiteVec,]$OBJECTID,"numeric")
FinalTable = data.frame(coordinates(Coral),Coral$CatalogNumber,
                        closestSiteVec,minDistVec,PointAssignEMU)
#
# Update the Temperature Assignment table column names 
#
names(FinalTable) <- c("Long","Lat","CatalogNumber","CloseEMUIndex","Distance","OBJECTID")
#
# And, at the end, write the point assignment file.
#
message("Write temperature/density assignment table to disk in .csv format")
write.csv(FinalTable,file="FinalEMUAssignments.csv")
