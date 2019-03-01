install.packages("rgdal")
install.packages("rworldmap")
install.packages("ncdf4")
install.packages("rworldxtra")
install.packages("raster")
install.packages("colorRamps")

library("ncdf4", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("rworldmap", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("rworldxtra", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("colorRamps", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#############     Pantanal shp     #############    
pantanal_shp_project <- readOGR("/PATH/Pantanal/Pantanal_shapefile/Pantanal.shp")

#############      Altenative Pantanal shp      #############     
pantanal_alternative_shp_project <- readOGR("/PATH/Pantanal/Alternative_shp_for_Pantanal/New_shp_for_Pantanal.shp")

#############  #############  #############      FILE_LIST in ESA DATA PATH     #############  #############  #############  
path_esa <- "/PATH/ESA_CCI_SM_v04.4_COMBINED/"
file_list <- list.files(path = path_esa, pattern = NULL, all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#############  ############# #############  FULL DATE LIST #############  #############  #############  
full_date_list <- seq(as.Date("2007/01/01"), as.Date("2017/12/31"), by = "day")

#############  ############# #############  read and use file
i = 10289            # the number of file which I want first (day) to use, located in ESA DATA PATH
                     # this is the 1st file which I will use

# I need to define first step in output file
esa_path_name_nc <- paste(path_esa,file_list[i],sep="")
print(esa_path_name_nc)
esa_nc <- nc_open(esa_path_name_nc)
year <- substr(file_list[i], 39, 42)
month <- substr(file_list[i], 43, 44)
day  <- substr(file_list[i], 45, 46)
date_file <- paste(year,"-",month,"-",day,sep="")
print(date_file)
date_file_v2 <- paste(year,month,day)
print(date_file_v2)

esa_lat <- ncvar_get(esa_nc,esa_nc$dim$lat)
esa_lon <- ncvar_get(esa_nc,esa_nc$dim$lon)
esa_sm <- esa_nc$var$sm
esa_sm_data <- ncvar_get( esa_nc, esa_sm)
dim(esa_sm_data)
fillvalue <- ncatt_get(esa_nc,esa_sm,"_FillValue")
esa_sm_data[esa_sm_data==-9999] <- NA

#############  #############  #############    RASTER  
esa_sm_ras = raster(esa_sm_data)
esa_ll <- extent( min(esa_lat), max(esa_lat),min(esa_lon), max(esa_lon))
extent(esa_sm_ras) <- esa_ll
esa_sm_r <- setExtent(esa_sm_ras, esa_ll, keepres=TRUE)
t_esa_sm_r = t(esa_sm_r)

#############  #############  #############    CROP MASK   
esa_sm_c <- crop(t_esa_sm_r,pantanal_alternative_shp_project) 
esa_sm_mask <- mask(esa_sm_c,pantanal_alternative_shp_project)
extent(esa_sm_mask)
crs(esa_sm_mask) <- "+proj=longlat +datum=WGS84  +ellps=WGS84"  
esa_sm_mask@data@unit <- "m3 m-3"
names(esa_sm_mask) <- "soil_moisture"
plot(esa_sm_mask)

#############  #############  #############            READ NC FILES            #############  #############  #############  
#############  #############  #############  ESA_CCI_SM_v04.4_COMBINED  NETCDF  #############  #############  #############  
i = 10290      # ( i = i + 1 ) here, I passed the second step ( 2nd day).
j = 2          # 2nd day
n <- 4018      # number of total file which I want use in loop processes ( 2007 - 2017, there are 4018 days)

esa_year <- seq(1,n,1)
esa_month <- seq(1,n,1)
esa_day <- seq(1,n,1)
esa_sm_avg <- seq(1,n,1)

outfile <- "esa_sm_pantanal_data_2007-2017.nc"

#############  #############  #############    for cycle     #############  #############  #############
for( a in 1:n ) {
  
  esa_path_name_nc <- paste(path_esa,file_list[i],sep="")
  esa_nc <- nc_open(esa_path_name_nc)
  
  year <- substr(file_list[i], 39, 42)
  month <- substr(file_list[i], 43, 44)
  day  <- substr(file_list[i], 45, 46)
  date_file <- paste(year,"-",month,"-",day,sep="")
  print(date_file)
  
  if ( full_date_list[j] == date_file )  {
    
    print(a)
    
    #############  #############  #############  lat lon
    esa_lat <- ncvar_get(esa_nc,esa_nc$dim$lat)
    esa_lon <- ncvar_get(esa_nc,esa_nc$dim$lon)
    
    esa_ll <- extent( min(esa_lat), max(esa_lat) ,  min(esa_lon), max(esa_lon) )
    
    #############  #############  #############  soil moisture 
    esa_sm <-  esa_nc$var$sm
    esa_sm_data <- ncvar_get( esa_nc, esa_sm)
    esa_sm_data[esa_sm_data==-9999] <- NA
    
    #############  #############  #############  raster
    esa_sm_ras = raster(esa_sm_data)
    extent(esa_sm_ras) <- esa_ll
    esa_sm_r <- setExtent(esa_sm_ras, esa_ll, keepres=TRUE)  

    #############  #############  #############  crop mask
    esa_sm_r_t = t(esa_sm_r)
    esa_sm_r_t_c <- crop(esa_sm_r_t,pantanal_alternative_shp_project) 
    esa_sm_r_t_c_mask <- mask(esa_sm_r_t_c,pantanal_alternative_shp_project)
    
    #############  #############  #############  info
    crs(esa_sm_r_t_c_mask) <- "+proj=longlat +datum=WGS84  +ellps=WGS84"
    esa_sm_r_t_c_mask@data@unit <- "m3 m-3"
    names(esa_sm_r_t_c_mask) <- "soil_moisture"

  esa_sm_mask <- stack(esa_sm_mask, esa_sm_r_t_c_mask)
  dates <- seq(full_date_list[1], full_date_list[j], by = "day")
  dene_date <- setZ(esa_sm_mask, dates)
  
  writeRaster(dene_date, outfile1, overwrite=TRUE, format="CDF", varname="sm", varunit="m3 m-3", 
              longname="Volumetric Soil Moisture", xname="lon", yname="lat", zname="time")
  
  i = i+1
  j = j+1               

  }  else {  letter <- paste(a,"No date/NA",full_date_list[j] )
  print(letter)
  
  j = j+1               
  
  }
}
