
library(rpart)
library(party)
library(tidyverse)


setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/CART")
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

d.selection$Family <- as.factor(d.selection$Family)

rpart.control

(tree.model = rpart(Family ~ EMU_salinity + EMU_appO2ut + EMU_percO2sat + DepthInMeters + EMU_temp_ +EMU_dissO2 + EMU_nitrate + EMU_phosphate + 
                   EMU_silicate + EMU_ChlorA_12yrAvg,data = d.selection, control = rpart.control(minbucket = 2000, maxdepth = 6)))

plot(tree.model)
text(tree.model)

plot(d.selection$EMU_ChlorA_12yrAvg, col = c("red", "blue", "forestgreen", "black")[d.selection$Order], xlab = "", ylab = "ChlorA_12yrAvg")
legend(120, 2, c("Alcyonacea", "Antipatharia", "Pennatulacea", "Scleractinia"), col = c("red", "blue", "forestgreen", "black"), pch = 1)
abline(h = 2.45)

printcp(tree.model)


ctree_control(mincriterion = 1)

ctree.model = ctree(Family ~ EMU_salinity + EMU_appO2ut + EMU_percO2sat + DepthInMeters + EMU_temp_ +EMU_dissO2 + EMU_nitrate + EMU_phosphate + 
                      EMU_silicate + EMU_ChlorA_12yrAvg,data = d.selection, control = ctree_control(minbucket = 16000, maxdepth = 4))
plot(ctree.model)