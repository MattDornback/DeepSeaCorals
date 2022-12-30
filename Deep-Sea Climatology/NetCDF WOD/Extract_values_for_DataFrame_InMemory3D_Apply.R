#Script to extract World Ocean Database Global Temperature Climatologies via downloaded netcdf file.
#PC System Requirements: Minimum of 8 GB of RAM and enough storage 

#load necessary packages
library(ncdf4)
library(ncdf4.helpers)
library(ncdf.tools)
library(dplyr)

#set my working directory to location of netcdf file
setwd("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology")

#Website for WODB Worldwide temperature climatology
#https://www.nodc.noaa.gov/cgi-bin/OC5/woa13/woa13.pl
#Select Availabe formats as NetCDF 
#Select Availabe grids as 1/4 degree
#Click "Update Data Links"
#Under "All fields data links (1/4 grid)" click on THREDDS Catalog
#Download 664.9 MB woa13_decay_t0004v2.nc for annual average of all time periods.

# Replace everthing between the ############ with your read.csv and data filter and try
##############################################################################################
indata <-read.csv("C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/DSCRTP_NatDB_20171214-0/DSCRTP_NatDB_20171214-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

mydata <-filt %>%
  select(CatalogNumber, ObservationYear, Latitude, Longitude, DepthInMeters)
##############################################################################################

#open connection to the downloaded netcdf file using ncdf4 package
wod_t <- ncdf4::nc_open('C:/Users/matt.dornback/R_Working_Directory/WOA13v2/woa13_decav_temp00_01v2.nc')
#open connection to the downloaded netcdf file using ncdf4 package
wod_s <- ncdf4::nc_open('C:/Users/matt.dornback/R_Working_Directory/WOA13v2/woa13_decav_salinity00_01v2.nc')

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

#close connections to wod_t and wod_s netcdf files
ncdf4::nc_close(wod_t)
ncdf4::nc_close(wod_s)

#create object tend by getting current time with Sys.time() function
#mycube should now be an in memory object as a large array nearly 807 Mb insize
tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed

#Create function to get x, y, z index values for a data row and extract temperature and salinityc values
#for the index values from mycube

get_dfrow_xyzt_v <- function(fx,fy,fz) {
  #Using which.min to find lons, lats, depths index values that minimizes the difference
  ix <- which.min (abs(fx - nc_x))
  iy <- which.min (abs(fy - nc_y))
  iz <- which.min (abs(fz - nc_z))
  
  lon_wodt <- nc_x[ix]
  lat_wodt <- nc_y[iy]
  depth_wodt <- nc_z[iz]
  t_an_wodt <- mytcube[ix, iy, iz]
  s_an_wodt <- myscube[ix, iy, iz]
  
  f <- c(t_an_wodt, s_an_wodt, lon_wodt, lat_wodt, depth_wodt, ix, iy, iz)
  return(f)
}

#Want to check how long things take
#create oject tstart by getting current time with Sys.time() function.
tstart <- Sys.time()
#show tstart in console
tstart

#Use the apply function to call get_dfrow_xyzt_v  to get indices for 3d array
#xyzt_v is a data frame coerced from transposed using the function t() and then
#coerced to a dataframe wit the as.data.frame()
xyzt_v <- as.data.frame(
  t(
    apply(mydata[,c('Longitude','Latitude','DepthInMeters')],
          1,
          function(x) get_dfrow_xyzt_v(x['Longitude'],x['Latitude'],x['DepthInMeters']))
  )
)

#Using the name() function to rename the columns in xyzt_v
names(xyzt_v) <- c('t_an_wodt','s_an_wodt','lon_wodt', 'lat_wodt', 'depth_wodt', 'ix', 'iy', 'iz')

#using cbind() function to append the mydata and xyzt_v dataframes together.
mydata <- cbind(mydata,xyzt_v)

tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed

write.csv(mydata, file="mydata_apply_method2.csv", row.names=FALSE)
