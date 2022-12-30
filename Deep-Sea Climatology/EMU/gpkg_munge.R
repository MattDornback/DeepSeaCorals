library(dplyr)

setwd('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology')

db <- src_sqlite("EMU.gpkg")
#list tables with src_tbls()
src_tbls(db)

emumaster <- tbl(src_sqlite("EMU.gpkg"),"EMU_Master")
print(emumaster,n=1)

#[1] "EMU_Master"                    "gpkg_contents"                 "gpkg_data_column_constraints"  "gpkg_data_columns"            
#[5] "gpkg_extensions"               "gpkg_geometry_columns"         "gpkg_metadata"                 "gpkg_metadata_reference"      
#[9] "gpkg_spatial_ref_sys"          "gpkg_tile_matrix"              "gpkg_tile_matrix_set"          "rtree_EMU_Master_Shape"       
#[13] "rtree_EMU_Master_Shape_node"   "rtree_EMU_Master_Shape_parent" "rtree_EMU_Master_Shape_rowid"  "sqlite_sequence"

library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),dbname="EMU.gpkg")
test <- dbGetQuery(con,"select * from EMU_Master where POINTID = 1")

test2 <- dbGetQuery(con,"select * from EMU_Master where depth_lvl = 1")

test3 <- dbGetQuery(con,"select * from EMU_Master")

test4 <- dbGetQuery(con,"select * from EMU_Master where SpecialCases = 'Bottom' ")
sal <- subset(test4,select=c(POINT_X,POINT_Y,salinity))

head(test4,1)

library(sp)
library(rgdal)
library(raster)

coordinates(sal) <- ~POINT_X+POINT_Y
gridded(sal) = TRUE
r <- raster(sal)
plot(r)

setwd('C:/NCEI_DataVis/MDornback')
dcs<- read.csv("DSC_Location_Sub.csv",stringsAsFactors = F)
coordinates(dcs) <- ~Longitude+Latitude

dcs$salinity <- extract(r,dcs)

gom <- test2[(test2$POINT_X > -98 & test2$POINT_X < -80) & (test2$POINT_Y > 18 & test2$POINT_Y < 31),]

print(emumaster, n = 1)

test <- as.data.frame(print(emumaster,n=5))

head(emumaster[1])

a <- tbl(src_sqlite("EMU.gpkg"),"gpkg_data_columns")
print(a)