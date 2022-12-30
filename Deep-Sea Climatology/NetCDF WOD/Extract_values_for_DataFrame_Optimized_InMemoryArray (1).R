#Script to extract World Ocean Database Global Temperature Climatologies via downloaded netcdf file.
#PC System Requirements: Minimum of 8 GB of RAM and enough storage 

#load necessary packages
library(ncdf4)
library(ncdf4.helpers)
library(ncdf.tools)


#set my working directory to location of netcdf file
setwd('c:/NCEI_DataVis/MDornback')

#Website for WODB Worldwide temperature climatology
#https://www.nodc.noaa.gov/cgi-bin/OC5/woa13/woa13.pl
#Select Availabe formats as NetCDF 
#Select Availabe grids as 1 degree
#Click "Update Data Links"
#Under "All fields data links (1/4 grid)" click on THREDDS Catalog
#Download 181 MB woa13_decay_t0001v2.nc for annual average of all time periods.


#Create the oject myfile to contain the text string of the file name and path
#myfile <- 'c:/NCEI_DataVis/MDornback/woa13_decav_t00_01v2.nc'

#Use the expand grid function create a data frame of simulated lon lat values.
#The result should be a dataframe with columns x and y
mydata <- expand.grid(x=seq(-97.5, -80.5, 1),y=seq(30.5, 18.5, -1))
#Use the sample function to add a random depth between 1 to 10 meters for each
#row in mydata.
mydata$z <- sample(0:10,nrow(mydata),replace=T)
#Use names() function to rename x, y, and z to the names below
names(mydata) <- c("Longitude","Latitude","Depth_m")


#open connection to the downloaded netcdf file using ncdf4 package
wod_t <- ncdf4::nc_open('c:/NCEI_DataVis/MDornback/woa13_decav_t00_01v2.nc')
wod_s <- ncdf4::nc_open('c:/NCEI_DataVis/MDornback/woa13_decav_s00_01v2.nc')

#Get list of variables in objet wod_t
names(wod_t$var)
#Get list of variables in objet wod_s
names(wod_s$var)

#extract dimensional values for lon, lat, depth and time using ncdf package ncvar_get() function.
#Each call of ncvar_get should result in a vector of values for each variable
#Time vector not of interest in this instance.
nc_x <- ncdf4::ncvar_get(wod_t,varid="lon")
nc_y <- ncdf4::ncvar_get(wod_t,varid="lat")
nc_z <- ncdf4::ncvar_get(wod_t,varid="depth")
nc_t <- ncdf4::ncvar_get(wod_t,varid="time")


#Want to check how long things take
#create oject tstart by getting current time with Sys.time() function.
tstart <- Sys.time()
#show tstart in console
tstart

#Read in 3D array of lon, lat and t_an (Objectively analized mean) from wod_t
#
mytcube <- ncdf4::ncvar_get(nc=wod_t,varid="t_an",start=c(1,1,1,1),
                           count=c(length(nc_x),length(nc_y),length(nc_z),length(nc_t)))
myscube <- ncdf4::ncvar_get(nc=wod_s,varid="s_an",start=c(1,1,1,1),
                            count=c(length(nc_x),length(nc_y),length(nc_z),length(nc_t)))
#create object tend by getting current time with Sys.time() function
#mycube should now be an in memory object as a large array nearly 807 Mb insize
tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed

#Want to check how long things take
#create oject tstart by getting current time with Sys.time() function.
tstart <- Sys.time()
#show tstart in console
tstart

for (i in 1:nrow(mydata)){
  
  #Using which.min and index values that minimizes the difference
  ix <- which.min (abs(mydata$Longitude[i] - nc_x))
  iy <- which.min (abs(mydata$Latitude[i] - nc_y))
  #find depth that minimizes the difference
  iz <- which.min (abs(mydata$Depth_m[i] - nc_z )) 
  
  #add actualy values to dataframe
  mydata$wod_lon[i] <- nc_x[ix]
  mydata$wod_lat[i] <- nc_y[iy]
  mydata$wod_depth[i] <- nc_z[iz]
  
  #use the ix, iy and iz index values to extract values from mycube
  
  mydata$wod_t_an[i] <- mytcube[ix,iy,iz]
  mydata$wod_s_an[i] <- myscube[ix,iy,iz]
  mydata$ix[i] <- ix
  mydata$iy[i] <- iy
  mydata$iz[i] <- iz

}

tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed

write.csv(mydata,file="testdf.csv")
