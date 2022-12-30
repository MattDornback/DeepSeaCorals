##### Header #####
# Author: Robert P. McGuinn
# Date started: 2018-08-14
# Purpose: Execute RMarkdown documents on dashboards for each DatasetID.  
#   4 groups: Cruise, Literature, Program, Repository

##### bring in dataset 'key' and transform #####
library(tidyr)
setwd("C:/rworking/digs/indata")
key <- read.csv('20171214-0_DatasetID_Key.csv', header = TRUE)
#get rid of some bad data
key <- key[-46,1:2]
# get rid of NA values
key <- key[is.na(key$DatasetID) == F,]
key <- key %>%
  separate(Full.DatasetID, c("class", "title"), ":")

##### primary subsetting of indata to d ##### 
d <- indata %>% 
  filter(
    #Flag == "0", 
         DatasetID == "Thoma_J_2013" |
           DatasetID == "MCZ-IZ" |
           DatasetID == "NOAA_EX-15-04-L2" |
           DatasetID == "NOAA_AFSC_Longline_Survey"
  )

##### merge d (primary subset) with key  ##### 
d <- merge(d, key, all.x = TRUE)

##### cruise: run RMD on each unique DatasetID group ##### 
#cruise subset
x <- d %>%
  filter(
    class == 'Cruise'
  )

# run RMD
library("rmarkdown")
for (id in unique(x$DatasetID)){
  sub <- x[x$DatasetID == id,]
  render("C:/rworking/digs/code/20180817_0_Cruise_DatasetID_Dash_HTML_RPMcGuinn.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}

##### literature: run RMD on each unique DatasetID group ##### 
x <- d %>%
  filter(
    class == 'Literature'
  )

library("rmarkdown")
for (id in unique(x$DatasetID)){
  sub <- x[x$DatasetID == id,]
  render("C:/rworking/digs/code/20180817_0_Literature_DatasetID_Dash_HTML_RPMcGuinn.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}


##### program: run RMD on each unique DatasetID group ##### 
x <- d %>%
  filter(
    class == 'Program'
  )

library("rmarkdown")
for (id in unique(x$DatasetID)){
  sub <- x[x$DatasetID == id,]
  render("C:/rworking/digs/code/20180817_0_Program_DatasetID_Dash_HTML_RPMcGuinn.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}


##### repository: run RMD on each unique DatasetID group ##### 
x <- d %>%
  filter(
    class == 'Repository'
  )

library("rmarkdown")
for (id in unique(x$DatasetID)){
  sub <- x[x$DatasetID == id,]
  render("C:/rworking/digs/code/20180817_0_Repository_DatasetID_Dash_HTML_RPMcGuinn.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}











