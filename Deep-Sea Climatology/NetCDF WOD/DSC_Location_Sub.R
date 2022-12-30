library(knitr)
library(rmarkdown)
library(dplyr)
library(magrittr)
library(ggmap)

setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology")

indata <-read.csv("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/DSCRTP_NatDB_20171214-0/DSCRTP_NatDB_20171214-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

data_categories <-filt %>%
  select(CatalogNumber, ObservationYear, Latitude, Longitude, DepthInMeters)

write.csv(data_categories, file="DSC_Location_Sub.csv", row.names=FALSE)

#pull records from specific column
column_records <- filt %>%
  filter(DatasetID == 'Thoma_J_2013')