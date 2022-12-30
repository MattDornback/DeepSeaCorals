library(caret)
library(tidyverse)
library(randomForest)

setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/RandomForest")
d <- read_rds('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Coral_EMU_20190709-3_Pacific.rds')

d.selection <- d %>% select(DepthInMeters, EMU_temp_, EMU_salinity, EMU_appO2ut, 
                            EMU_dissO2, EMU_nitrate, EMU_percO2sat, EMU_phosphate, 
                            EMU_silicate, EMU_ChlorA_12yrAvg, Phylum, Class, Order, Family, FishCouncilRegion, 
                            EMU_Cluster37, EMU_GeomorphologyBase, CatalogNumber, ObservationYear, Ocean, SamplingEquipment) %>% 
  filter(Ocean == "North Pacific",
         SamplingEquipment %in% c("AUV","drop camera", "ROV", "SCUBA", "submersible", "towed camera", "trawl"),
         ObservationYear > 1977,
         Class %in% c("Anthozoa"), 
         Order %in% c("Alcyonacea", "Antipatharia", "Pennatulacea", "Scleractinia"), 
         !EMU_Cluster37 %in% c(0,11,14,25)) %>% 
  na.omit()

d.selection$Order <- as.factor(d.selection$Order)

(RF.model = randomForest(Family ~ EMU_salinity + EMU_appO2ut + EMU_percO2sat + DepthInMeters + EMU_temp_ +EMU_dissO2 + EMU_nitrate + EMU_phosphate + 
                   EMU_silicate + EMU_ChlorA_12yrAvg, distribution = "multinomial",data = d.selection))
importance(RF.model)
