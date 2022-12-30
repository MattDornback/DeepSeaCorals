library(sp)
library(tidyverse)
library(leaflet)
library(extrafont)
library(RColorBrewer)
library(googlesheets)
library(googledrive)
library(maps)
library(rgdal)
library(raster)
library(spocc)
library(reshape2)

setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/NMDS")
d <- read_rds('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology/EMU/Pacific_Coral_EMU.rds')

d <- d %>%
  filter(
         ObservationYear != "-999",
         DepthInMeters != "-999", 
         is.na(Phylum)== F, EMU_Cluster37 %in% c(3,13,19,33,37))
options(digits = 1)

# changing levels of 'Ecoregion'
#d <- d %>% mutate(Ecoregion = ifelse(Ecoregion == 'N_Carolinean', 'North Carolinian', as.character(Ecoregion)))
#d <- d %>% mutate(Ecoregion = ifelse(Ecoregion == 'South Carolinean', 'South Carolinian', as.character(Ecoregion)))

##### setting depth class DepthCat4 - 4 class ##### 
d$DepthCat4[d$DepthInMeters < 1000] <- "< 1000 m"
d$DepthCat4[d$DepthInMeters > 1000 & d$DepthInMeters <= 2000] <- "1000-2000 m"
d$DepthCat4[d$DepthInMeters > 2000 & d$DepthInMeters <= 3000] <- "2000-3000 m"
d$DepthCat4[d$DepthInMeters > 3000] <- "> 3000 m"
d$DepthCat4 <- factor(d$DepthCat4, levels = c("< 1000 m", "1000-2000 m","2000-3000 m", "> 3000 m" ))


###### Figure: NMDS results on 12 sites (4 Depth zones, by 3 Ecoregions)

##### start empty subset varialble #####
d$rep <- NA

##### summarize unique genera and set threshold#####
sum_tbl <-
  as.data.frame(d) %>%
  group_by(Genus) %>%
  summarize(
    Records = n()
  )
sum_tbl <- sum_tbl %>%
  filter(Records > 30)

# select threshold for number of occurrences 
# over the project area of interest.
##### Select only genera to be included as specfied above #####
d2 <- d %>%
  filter(Genus %in% sum_tbl$Genus)

# setdiff(unique(d2$Genus), genera_1)
# setdiff(genera_1, unique(d2$Genus))

##### Populate IndividualCount with 1 #####

d2$IndividualCount = 1

##### create site X species matrix #####
library(vegan)
site.sp <- dcast(d2, EMU_Cluster37 + DepthCat4 ~ Genus)

# setwd("C:/rworking/digs/outdata")
# write.csv(site.sp, 'site.sp.csv')

# creating a site variable
site.sp$site <- paste(site.sp$EMU_Cluster37, site.sp$DepthCat4, sep = '|')

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EMU_Cluster37, -DepthCat4)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)

# # limit to sites that actually have more than X observations
# site.sp <- site.sp[apply(site.sp, 1, sum) > 200,]
# dim(site.sp)

# making site numeric
# site.sp$site <- as.numeric(factor(site.sp$site))
# str(site.sp)


##### running NMDS starting with site X species matrix #####
#install.packages("vegan")
library(vegan)
NMDS <- metaMDS(site.sp, distance = "jaccard", binary = T, k=2)

##### creating a group variable for Ecoregion ##### 
site.sp2 <- dcast(d2, EMU_Cluster37 + DepthCat4 ~ Genus) 
EMU_Cluster37 <- site.sp2$EMU_Cluster37

##### extracting the site and species scores for use with ggplot2 ##### 
# extracting site scores 
site.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
site.scores$EMU_Cluster37 <- EMU_Cluster37  #  add the Ecoregion variable created earlier
head(site.scores)  #look at the data

# extracting species scores
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

##### create a hull around the grouping variable Ecoregion #####
Ecoregion.a <- site.scores[site.scores$EMU_Cluster37 == 3, ][chull(site.scores[site.scores$EMU_Cluster37 == 3, c("NMDS1", "NMDS2")]), ]  # hull values for Ecoregion A
Ecoregion.b <- site.scores[site.scores$EMU_Cluster37 == 13, ][chull(site.scores[site.scores$EMU_Cluster37 == 13, c("NMDS1", "NMDS2")]), ]  # hull values for Ecoregion B
Ecoregion.c <- site.scores[site.scores$EMU_Cluster37 == 33, ][chull(site.scores[site.scores$EMU_Cluster37 == 33, c("NMDS1", "NMDS2")]), ]  # hull values for Ecoregion A

hull.data <- rbind(Ecoregion.a, Ecoregion.b, Ecoregion.c)  #combine groups
hull.data

##### creating a group variable for depthCat##### 
site.sp2 <- dcast(d2, EMU_Cluster37 + DepthCat4 ~ Genus)
Depth_Zone <- site.sp2$DepthCat4

##### extracting the species scores for use with ggplot2 ##### 
# extracting site scores 
site.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
site.scores$Depth_Zone <- Depth_Zone  #  add the Depth_Zone variable created earlier
head(site.scores)  #look at the data

# extracting species scores
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

##### create a hull around the grouping variable DepthCat ##### 
Depth_Zone.a <- site.scores[site.scores$Depth_Zone == "< 150 m", ][chull(site.scores[na.omit(site.scores$Depth_Zone) == 
                                                                                       "< 150 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone A
Depth_Zone.b <- site.scores[site.scores$Depth_Zone == "150-300 m", ][chull(site.scores[na.omit(site.scores$Depth_Zone) == 
                                                                                         "150-300 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone B
Depth_Zone.c <- site.scores[site.scores$Depth_Zone == "300-600 m", ][chull(site.scores[na.omit(site.scores$Depth_Zone) == 
                                                                                         "300-600 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone A
Depth_Zone.d <- site.scores[site.scores$Depth_Zone == "> 600 m", ][chull(site.scores[na.omit(site.scores$Depth_Zone) == 
                                                                                       "> 600 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone B
hull.data <- rbind(Depth_Zone.a, Depth_Zone.b, Depth_Zone.c, Depth_Zone.d)  #combine groups
hull.data

##### cleaned up plot with hulls groups = DepthCat ##### 
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Depth_Zone,group=Depth_Zone),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            alpha=0.5, 
            fontface = "bold", 
            size=3.2 #,
            #position=position_jitter(width=.5,height=.5)
  ) +  # add the species labels
  # geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,colour=Depth_Zone),size=4) +
  #   scale_colour_manual(values = c('< 150 m' = 'blue',
  #                                '150-300 m' = 'red', 
  #                                '300-600 m' = 'green', 
  #                                '> 600 m' = 'black')) +
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=factor(EMU_Cluster37),size=3)) + # add the point markers
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2), shape = 3) + 
  coord_equal() +
  theme_bw(base_size = 15, base_family = "Cambria") +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank());

```

###### Figure: All ecoregions, all depths.  

sum_tbl <-
  as.data.frame(d) %>%
  group_by(Genus) %>%
  summarize(
    Phylum = toString(unique(Phylum)),
    Taxon_Rank = toString(unique(TaxonRank)),
    ScientificNames = toString(unique(ScientificName)),
    Records = n(),
    MinDepth = min(DepthInMeters),
    MedianDepth = median(DepthInMeters),
    MaxDepth = max(DepthInMeters)) %>%
  arrange(Records)
options(scipen=10000)
#View(sum_tbl)

sum_tbl <- sum_tbl %>%
  filter(Records > 25)

d2 <- as.data.frame(d) %>%
  filter(Genus %in% genera, 
         Phylum == "Cnidaria", 
         #Ecoregion == 'Floridian',#,
         #DepthCat == 'very shallow'
         as.numeric(DepthInMeters) > 0,
         as.numeric(DepthInMeters < 1250))

g <- ggplot(d2, aes(reorder(Genus, DepthInMeters, FUN=median), as.numeric(DepthInMeters),fill=Order)) +   
  geom_boxplot() +
  scale_y_reverse() +
  ylab("Depth (meters)") + 
  xlab("Genus") +
  theme_bw(base_size = 22, base_family = "Cambria") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, face = 'italic'))  +
  #  geom_hline(yintercept = 150, col = 'grey') +
  geom_hline(yintercept = 300, col = 'grey') #+
#  geom_hline(yintercept = 600, col = 'grey')

set.seed(8)
g + scale_fill_manual(values = sample(big_palette))
#g + scale_color_manual(values = brewer.pal(12, "Paired")[c(10,9,8,7,6,5,4,3,2,1)])

# Pie charts
#####
set.seed(3)
# build a color matching data frame
colormatchdf <- data.frame(Genus = genera_2, color = sample(big_palette[1:19], length(genera_2)))
#get a summary vector of Genus Counts
GenusCounts <- summary(d2$Genus)
# limit by count
GenusCounts <- GenusCounts[GenusCounts>50]
# make count into data frame
GenusCounts <- data.frame(Genus=names(GenusCounts), value=GenusCounts, row.names=NULL)
# merge to get color field into cound summary dataframe
GenusCountsColor <- merge(GenusCounts, colormatchdf, by = "Genus")
# sort by value to make prettyer chart
GenusCountsColor <- GenusCountsColor[order(as.numeric(GenusCountsColor$value)),]
# make color vector
colors <- as.vector(GenusCountsColor$color)
# set pallete
palette(colors)
# make pie chart
pie(GenusCountsColor$value,labels = GenusCountsColor$Genus, col = c(1:length(GenusCountsColor$value)), cex = .7)