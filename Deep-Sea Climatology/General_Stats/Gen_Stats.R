library(tidyverse)
library(leaflet)

setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/General_Stats")
d <- read_rds('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Coral_EMU_20190709-3_Pacific.rds')

EMU <- read_csv('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Pacific_EMU_Bottom.csv')

Coral <- d %>% filter(Ocean == "North Pacific",
                      SamplingEquipment %in% c("AUV","drop camera", "ROV", "SCUBA", "submersible", "towed camera", "trawl"),
                          ObservationYear > 1977, 
                      Class %in% c("Anthozoa"), 
                      Order %in% c("Alcyonacea"#, "Antipatharia", "Pennatulacea", "Scleractinia"
                                   ),
                        Family %in% c("Acanthogorgiidae","Alcyoniidae","Chrysogorgiidae","Coralliidae","Isididae",
                                      "Paragorgiidae","Plexauridae","Primnoidae")) 

Coral2 <- Coral %>% mutate(EMU_GeomorphologyBase=replace(EMU_GeomorphologyBase, EMU_GeomorphologyBase=="Abyssal Hills", "Abyssal Mountains"),
                            EMU_GeomorphologyBase=replace(EMU_GeomorphologyBase, EMU_GeomorphologyBase=="Abyssal Mountians", "Abyssal Mountains"),
                            EMU_GeomorphologyBase=replace(EMU_GeomorphologyBase, EMU_GeomorphologyBase=="medium Shelf", "Shelf"),
                            EMU_GeomorphologyBase=replace(EMU_GeomorphologyBase, EMU_GeomorphologyBase=="high Shelf", "Shelf"),
                            EMU_GeomorphologyBase=replace(EMU_GeomorphologyBase, EMU_GeomorphologyBase=="low Shelf", "Shelf")) %>%
   filter(!EMU_GeomorphologyBase %in% c(NA, "Abyssal Plains")) %>% 
  group_by(Family, EMU_GeomorphologyBase) %>% 
  summarize(count = n()) %>% 
  mutate(Fam_Base = paste(Family,EMU_GeomorphologyBase, sep = '_'))
Coral_sum <- aggregate(Coral2$count, by=list(Family=Coral2$Family), sum)
Coral2 <- left_join(Coral2, Coral_sum, by = "Family")

EMU <- EMU %>% filter(depth_lvl > 10)

C_cluster <- as.data.frame(table(Coral$EMU_Cluster37))

E_cluster <- as.data.frame(table(EMU$Cluster37))

Event_cluster <- with(Coral, aggregate(cbind(EventID) ~ EMU_Cluster37, FUN =function(x){length(unique(x))}))
Event_cluster$EMU_Cluster37 <- as.character(Event_cluster$EMU_Cluster37)

CE <- right_join(C_cluster, E_cluster, by = "Var1")
CE[is.na(CE)] <- 0
CE <- rename(CE, EMU_Cluster37 = Var1, Coral = Freq.x, EMU = Freq.y)
CE <- left_join(CE, Event_cluster, by = "EMU_Cluster37")
CE <- na.omit(CE)

CE <- CE %>% filter(EventID > 100, EMU_Cluster37 %in% c(3,13,33,37))

CE <- mutate(CE, 
             CoralpDive = Coral / EventID,
             CoralpEMU = Coral / EMU,
             EMU_Percent = EMU/sum(EMU)*100, Coral_Percent = Coral/sum(Coral)*100, Dive_Percent = EventID/sum(EventID)*100)
#CE  <- mutate(CE, Dive_Perc_Norm = (Coral_Percent/Dive_Percent), 
#              EMU_Perc_Norm = (Coral_Percent/EMU_Percent), CpD_Percent = CoralpDive/sum(CoralpDive)*100)

# make Cluster an ordered factor
CE$EMU_Cluster37 <- factor(CE$EMU_Cluster37, levels = CE$EMU_Cluster37)

ggplot(CE, aes(EMU_Cluster37)) + geom_bar(aes(weight = CoralpDive)) + ggtitle("Corals per Dive") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
  plot.theme + xlab("EMU Cluster")
ggplot(CE, aes(EMU_Cluster37)) + geom_bar(aes(weight = CoralpEMU)) + ggtitle("Corals per EMU")
ggplot(CE, aes(Cluster)) + geom_bar(aes(weight = EMU_Perc_Norm))





# Cleaning notes
# remove <50m, non-sub/rov/drop-cam
table(C3$SamplingEquipment)
EMU <- rename(EMU, longitude = POINT_X, latitude = POINT_Y)
C3 <- EMU[Coral$EMU_Cluster37 == 18,]
leaflet(data =C3) %>%
  addTiles() %>%
  addMarkers()


P_EMU <- EMU %>% filter(OceanName %in% c("North Pacific", "South Pacific"))

plot.theme <- theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                               size = 12, angle = 0),
                    axis.text.y = element_text(face = "bold", color = "blue", 
                                               size = 12),
                    plot.title = element_text(hjust = 0.5))#,
                    #axis.title.x = element_blank())



ggplot(Coral, aes(x=Family, y=EMU_ChlorA_12yrAvg)) + geom_boxplot() + 
  ggtitle("Chlor a Distribution of Order Alcyonacea by Family") + 
  coord_cartesian(ylim = c(0, 7)) + plot.theme + ylab("Chlor a (mg m-3)")
ggplot(Coral, aes(x=Family, y=EMU_nitrate)) + geom_boxplot() + plot.theme + 
  ggtitle("Nitrate Distribution of Order Alcyonacea by Family") + ylab("Nitrate (umol/l)")
ggplot(Coral, aes(x=Family, y=DepthInMeters)) + geom_boxplot() + plot.theme + 
  ggtitle("Depth Distribution of Order Alcyonacea by Family") + ylab("Depth (m)")
ggplot(Coral, aes(x=Family, y=EMU_appO2ut)) + geom_boxplot() + plot.theme + 
  ggtitle("App O2 Utilization Distribution of Order Alcyonacea by Family") + ylab("App O2 Util (ml/l)")
ggplot(Coral, aes(x=Family, y=EMU_silicate)) + geom_boxplot() + plot.theme + 
  ggtitle("Silicate Distribution of Order Alcyonacea by Family") + ylab("Silicate (umol/l)")

ggplot(Coral, aes(x=Family, y=EMU_temp_)) + geom_boxplot() + plot.theme
ggplot(Coral, aes(x=Family, y=EMU_salinity)) + geom_boxplot() + plot.theme
ggplot(Coral, aes(x=Family, y=EMU_percO2sat)) + geom_boxplot() + plot.theme
ggplot(Coral, aes(x=Family, y=EMU_nitrate)) + geom_boxplot() + plot.theme

ggplot(Coral2, aes(x=Family, y=count/x, fill=EMU_GeomorphologyBase)) + 
  geom_bar(stat = "identity",position = "dodge")+ plot.theme + ylab("Percent of Family Total") +
  ggtitle("Geomorphology Distribution by Family")+
  plot.theme +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), legend.title = element_blank())


