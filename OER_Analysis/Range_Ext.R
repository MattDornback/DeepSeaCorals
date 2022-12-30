#Dependencies
library(tidyverse)
library(robis)
library(geosphere)
library(leaflet)

#Import test data from SeaTube csv file
ST_data <- read.csv('C:/Users/matt.dornback/R_Working_Directory/OER_Analysis/SeaTubeAnnotations_EX1811_Test.csv')


###PUll made up points from imported data for testing comparison, this section can be thrown out when the code is operational.

#Filter for specific organism
obs <- ST_data %>% filter(Annotation.ID == 5911150)

#Point observation that is nearby
obs_near <- ST_data %>% filter(Annotation.ID == 5911150) %>% select(DEEPDISCOVERERNAV01_23975_Longitude, DEEPDISCOVERERNAV01_23975_Latitude)

#What if an organism was found in the Gulf of Guinea far away
obs_far <- ST_data %>% filter(Annotation.ID == 5911151) %>% select(DEEPDISCOVERERNAV01_23975_Longitude, DEEPDISCOVERERNAV01_23975_Latitude)

###Compare the occurrence to existing OBIS data for that organism

#Pull information on specific creature from OBIS
OBIS_data <- occurrence(obs$Taxon)

#Trim everything but lat/lon from data for distance comparison
data_Loc <- OBIS_data %>%  select(decimalLongitude, decimalLatitude)

#Compare OBIS points (data_Loc) to new observation (obs_near or obs_far) to determine distance in meters
dist_mat <- distm(data_Loc, obs_far, fun = distGeo)
dist_mat

#Determine if all distances exceed a range of 1000 km (1e+06 m)
if (all(dist_mat >= 1e+06)) {
  print(paste('Organism range extension by ', round(min(dist_mat)/1000), 'km!', sep = ''))
  #Create a map of existing points with a marker for the 
  leaflet(OBIS_data) %>% addProviderTiles(providers$Esri.OceanBasemap)%>% addCircles(lng = ~decimalLongitude, lat = ~decimalLatitude) %>% addMarkers(lng=obs_far$DEEPDISCOVERERNAV01_23975_Longitude, lat=obs_far$DEEPDISCOVERERNAV01_23975_Latitude, popup=paste(obs$Taxon,'range extension'))
} else {
  print('Organism within normal range.')
}

#### TO DO:
#Label map with organism
#Save record to file if extension



#Pull from csv made up a normal and deep depths for this organism in meters. Can be thrown out when code is operational.
depth_n <- ST_data %>% filter(Annotation.ID == 5911150) %>% select(SBECTD9PLUSDEEPDISCOVERER_23978_Depth)
depth_d <- ST_data %>% filter(Annotation.ID == 5911151) %>% select(SBECTD9PLUSDEEPDISCOVERER_23978_Depth)


#Calculate the diff between the 5th and 95th percentiles of OBIS depths and the new organism depth in meters
q <- quantile(OBIS_data$depth, na.rm = TRUE, probs = c(0.05,0.95))
depth_diff <- sapply(q, function(x) abs(x - depth_d))

#Determine if all depth differences exceed a range of 100m
if (all(depth_diff >= 100, na.rm = TRUE)) {
  print(paste('Organism depth extension by ', min(unlist(depth_diff), na.rm = TRUE), 'm!', sep = ''))
  #Create a boxplot with existing data and a red dot for the outlyer data
  ggplot(OBIS_data, aes(x='OBIS Data', y=depth)) + 
    geom_boxplot(fill='blue') + 
    geom_jitter(position=position_jitter(0.2)) +
    geom_point(data = depth_d, aes(y=SBECTD9PLUSDEEPDISCOVERER_23978_Depth), color= 'red', size=4)
} else {
  print('Organism within normal depth range.')
}

#### TO DO:
#Label graph with organism
#Save record to file if extension


