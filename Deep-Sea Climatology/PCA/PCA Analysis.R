library(dplyr)
library(readr)
library(ggbiplot)
library(ggplot2)

#A helpful tutorial on the PCA analysis
#https://www.datacamp.com/community/tutorials/pca-analysis-r

#set working directory
setwd('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/PCA')
#file path to corals data and EMU data
Coral_path <- "C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Pacific_Coral_EMU.rds"

# read in coral data
d <- read_rds(Coral_path) 



# Select specific data to use, omit na's
d.selection <- d %>% select(DepthInMeters, EMU_ChlorA_12yrAvg, EMU_nitrate, EMU_silicate, EMU_appO2ut, EMU_temp_, EMU_salinity, 
           EMU_dissO2, EMU_percO2sat, EMU_phosphate, 
            Class, Order,Family, Genus,FishCouncilRegion, EMU_Cluster37, EMU_GeomorphologyBase, CatalogNumber,
           SamplingEquipment, ObservationYear, Ocean) %>% 
  filter(Ocean == "North Pacific",
         SamplingEquipment %in% c("AUV","drop camera", "ROV", "SCUBA", "submersible", "towed camera", "trawl"),
         ObservationYear > 1977,
         Class %in% c("Anthozoa"), 
         Order %in% c("Alcyonacea"#, "Antipatharia", "Pennatulacea", "Scleractinia"
                      ), 
         Family %in% c(#"Acanthogorgiidae"#,
           #"Alcyoniidae"#,
           #"Chrysogorgiidae"#,
           #"Coralliidae",
           #"Isididae"#,
           #"Paragorgiidae"#,
           #"Plexauridae"#,
           "Primnoidae"
                       ))

#select just genus with enough records
#Genus_picks <- d.selection %>% group_by(Genus) %>% summarize(count=n()>10) %>% filter(count %in% TRUE)

#select EMU_Cluster with enough records
#Cluster_picks <- d.selection %>% group_by(EMU_Cluster37) %>% dplyr::summarize(count=n()>500) %>% filter(count %in% TRUE)

#finish the filtering for EMUs
d.selection <- d.selection %>% filter(EMU_Cluster37 %in% c(3,13,33,37)) %>% na.omit() %>%
  dplyr::rename(Temp='EMU_temp_',Salinity='EMU_salinity',appO2u='EMU_appO2ut',dissO2='EMU_dissO2',
         Nitrate='EMU_nitrate',O2sat='EMU_percO2sat',Phosph='EMU_phosphate',
         Silicate='EMU_silicate',Chlor='EMU_ChlorA_12yrAvg',Depth="DepthInMeters")
#c(Cluster_picks$EMU_Cluster37)
# Principal component analysis using the first 10 columns for the analysis. Scale and center the data.
Coral.pca <- prcomp(d.selection[,c(1:5)], center = TRUE,scale. = TRUE)

# Show summary of importance of components
summary(Coral.pca)

# Show breakdown of each component
Coral.pca$rotation[,1:4]

#table of number of records in each EMU
table(d.selection$EMU_Cluster37)

# make Cluster an ordered factor
d.selection$EMU_Cluster37 <- as.factor(d.selection$EMU_Cluster37)
#d.selection$Family <- as.factor(d.selection$Family)
# Plot the PCA with ellipses around the determined groups. Scale the variables to make a more concise graphic.
ggbiplot(Coral.pca, var.axes = T,var.scale = 1,obs.scale = 1,circle=F,alpha=0.7,varname.size= 4,groups = d.selection$EMU_Cluster37)+
  scale_colour_manual(name="EMU", values= c("green", "red", "dark blue", "purple"))+
  ggtitle(paste("PCA Covariance of Family",d.selection$Family))+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) #+
  #scale_color_discrete(name = "EMU")
ggbiplot(Coral.pca, ellipse = T, var.scale = 1,obs.scale = 1,groups = d.selection$Genus)+
  #scale_colour_manual(name="Genus"#, values= c("green", "red", "dark blue", "purple"))+
  ggtitle(paste("PCA Covariance of Family",d.selection$Family))+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  scale_color_discrete(name = "Genus")
#ggbiplot(Coral.pca,obs.scale = 1, var.scale = 1)
#,obs.scale = 1
#,var.axes = FALSE
#, ellipse = T

ggplot(Coral, aes(EMU_Cluster37, label = factor(EMU_Cluster37))) + geom_bar()
