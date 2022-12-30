#Script to extract World Ocean Database Global Temperature Climatologies via downloaded netcdf file.
#PC System Requirements: Minimum of 8 GB of RAM and enough storage 

#load necessary packages
library(ncdf4)
library(ncdf4.helpers)
library(ncdf.tools)


#set my working directory to location of netcdf file
setwd('C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/Deep-Sea Climatology')

#Website for WODB Worldwide temperature climatology
#https://www.nodc.noaa.gov/cgi-bin/OC5/woa13/woa13.pl
#Select Availabe formats as NetCDF 
#Select Availabe grids as 1/4 degree
#Click "Update Data Links"
#Under "All fields data links (1/4 grid)" click on THREDDS Catalog
#Download 664.9 MB woa13_decay_t0004v2.nc for annual average of all time periods.

# Read in Data 
##############################################################################################
mydata <- read.csv("DSC_Location_Sub.csv",stringsAsFactors = F)
#Use names() function to rename x, y, and z to the names below
#names(mydata) <- c("Longitude","Latitude","DepthInMeters")
##############################################################################################

#mydata <- mydata[3000:3005,]
#mydata$DepthInMeters[1] <- 7000

#open connection to the downloaded netcdf file using ncdf4 package
wod_t <- ncdf4::nc_open('C:/Users/matt.dornback/R_Working_Directory/WOA13v2/woa13_decav_t00_04v2.nc')
#open connection to the downloaded netcdf file using ncdf4 package
wod_s <- ncdf4::nc_open('C:/Users/matt.dornback/R_Working_Directory/WOA13v2/woa13_decav_s00_04v2.nc')

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
#for the index values from mycube based on closes values fron nc_? vectors
#The function here has been modified to find the closest non missing temperature and salinity at depth for a given 
#lon, lat and depth.

get_dfrow_xyzt_v <- function(fx,fy,fz) {
  #Using which.min to find lons, lats, depths index values that minimizes the difference
  ix <- which.min (abs(fx - nc_x))
  iy <- which.min (abs(fy - nc_y))
  iz <- which.min (abs(fz - nc_z))
  
  #create empty datframe with total rows equal to length of nc_z
  z_t_s <- data.frame(z   = integer(length(nc_z)),
                         t = numeric(length(nc_z)),
                         s = numeric(length(nc_z))
                      )
  #fill in the empty dataframe with all depths in netcdf files
  #set z equalt to nc_z depths
  z_t_s$z <- nc_z
  #set t and s equal to vectors of temp/salinity from o to 5500 for a lon and lat determined above with lon_wod and lat_wod
  z_t_s$t <- mytcube[ix,iy,1:102]
  z_t_s$s <- myscube[ix,iy,1:102]
  #keep only complete cases where temp and salinity are not NA
  #this only keeps depth with temp and salinity values
  z_t_s <- z_t_s[complete.cases(z_t_s),]
  
  #extract word ocean databae lon, lat, depth, temp and salinity for a location and depth
  lon_wod <- nc_x[ix]
  lat_wod <- nc_y[iy]
  depth_wod <- nc_z[iz]
  t_an_wod <- mytcube[ix, iy, iz]
  s_an_wod <- myscube[ix, iy, iz]
  
  #set wod depth to NA if no temp and salinity data
  if (is.na(t_an_wod) && is.na(s_an_wod)) {
    depth_wod <- NA
  } 
  
  #find closet depth with valid non missing temperature and salinity salinity
  #the if logic is needed as some locations (land) do not have any valid data and these cases
  #need to be handled.
  if (nrow(z_t_s) != 0) {
    #determines closest depth with valid temp and salinity
    ciz <- which.min(abs(fz - z_t_s$z))
    #extracts the values
    c_depth_wod <- z_t_s$z[ciz]
    c_t_an_wod <-  z_t_s$t[ciz]
    c_s_an_wod <-  z_t_s$s[ciz]
    } else {
    #if no data for a location set everyting to NA
    ciz = NA
    c_depth_wod <- NA
    c_t_an_wod <-  NA
    c_s_an_wod <-  NA
    }

  f <- c(t_an_wod, c_t_an_wod, s_an_wod, c_s_an_wod, lon_wod, lat_wod, depth_wod, c_depth_wod, ix, iy, iz, ciz)
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
names(xyzt_v) <- c('t_an_wod', 'c_t_an_wod', 's_an_wod', 'c_s_an_wod',
                   'lon_wod', 'lat_wod', 'depth_wod', 'c_depth_wod', 'ix', 'iy', 'iz','ciz')

#using cbind() function to append the mydata and xyzt_v dataframes together.
mydata <- cbind(mydata,xyzt_v)

tend <-Sys.time()
#show tend in close
tend
#calculate elapsed time to create mycube array
elapsed <- (tend - tstart)
elapsed

write.csv(mydata,"DSC_Location_Sub_T_and_S.csv",row.names=F)


