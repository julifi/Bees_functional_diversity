## Global Setting ####################################################
install.packages("rdwd")
install.packages("RCurl")
install.packages("lubridate")
install.packages("terra")

# BEFORE: run setting.R
getwd()


## Load Libraries ############################################################# 
library(rdwd); library(RCurl); library(dplyr); library(readxl); library(terra); library(lubridate); 
library(tidyr)
#rdwd::updateRdwd()


### 1. check available .nc-files on the dwd FTP server -------
# currently available files in a given folder:

# 1.1 Temperature data: 
rasterbase <- paste0(gridbase,"/hourly/hostrada")
ftp.files <- indexFTP("/air_temperature_mean", base=rasterbase, dir=tempdir())

## example: temperature data: 
## .nc files containing hourly temperature data for one month: 
# The name of the NetCDF file is formed as follows:
# parameter abbreviation}_{time resolution}_{process name version}_{variant}_{grid_info}_{time coverage}.nc
# (variant: BE - best estimate; grid_info: gn - native grid), e.g.
# tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc

## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)
# --> 5th day, 2am (14 o'clock) = (5-1)*24+14 = 110
# --> xth day, y o'clock = (x-1)*24 + y

link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025030100-2025033123.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=3)

# 1.2 Precipitation data: ASCII Format

##
##
# to be done
##
##
##


# 1.3 reference raster
link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025040100-2025043023.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)

# set reference raster: rad[[1]]
raster <- rad[[1]]


### 2. load spatial data of TERENO sites --------------
# data: site_descriptions.xlsx
sites <- read_excel("analysis_bees_diversity/data/data_raw/site_descriptions.xlsx", sheet = "site_descriptions")
sites <- sites[c("SITE", "TRAP", "YEAR", "coordinates", "...5")]
sites  <- dplyr::rename(sites, "Site" = "SITE")
sites  <- dplyr::rename(sites, "Trap" = "TRAP")
sites  <- dplyr::rename(sites, "Year" = "YEAR")
sites  <- dplyr::rename(sites, "lon" = "coordinates")
sites  <- dplyr::rename(sites, "lat" = "...5")
# remove empty top rows  
sites  <- sites[3:nrow(sites),]

sites_2010 <- dplyr::filter(sites, Year == 2010)
sites_2010 <- sites_2010[c("Trap", "lon", "lat")]
sites_2010 <- sites_2010[,c(3,2,1)]
sites_2010$lat <- as.numeric(sites_2010$lat)
sites_2010$lon <- as.numeric(sites_2010$lon)
sites_2010 <- as.data.frame(sites_2010)

# convert site point data into SpatVector
sites_s <- vect(sites_2010, geom = c("lon", "lat"), crs="+proj=longlat")
plot(sites_s)
# use projection of reference raster (ETRS89-extended / LCC Europe --> EPSG: 3034)
sites_proj <- project(sites_s, "EPSG:3034")
plot(sites_proj)

# extract grid cell IDs of sites 
cell_id <- cellFromXY(raster, crds(sites_proj))  

# add colum with cell_id to sites data.frame:
sites_id_2010 <- cbind(sites_2010, cell_id)


### 3. choose relevant raster files (hourly resolution) on temp & precipitation from .nc files  -----------------------
# depending on: exact sampling days, daytime

# to DO: refine sampling dataset with respect to daytim!!!
# load data on daytime hours: 
daytime_hours <- read_excel("analysis_bees_diversity/data/data_raw/data_climate/daytime_hours.xlsx", sheet = "daytime_hours")
daytime_hours <- daytime_hours[c("sunrise_hour", "sunset_hour")]
daytime_hours$sunrise_hour <- format(daytime_hours$sunrise_hour, "%H:%M:%S")
daytime_hours$sunset_hour <- format(daytime_hours$sunset_hour, "%H:%M:%S")


# read prepared sampling data:
data_sampling <- readRDS("analysis_bees_diversity/data/sampling_days_siteyseason.RData")
data_samp_clim <- data_sampling

# test data on sampling period:
test_sampling <- read_excel("analysis_bees_diversity/data/data_raw/data_climate/test_sampling_period.xlsx")

# add column with 'hour'; replicate each row 23times --> for 'hour' passing values from 00:00:00 to 23:00:00
# create vector with values from 00:00 to 23:00 
z <- seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 hour")
z <- format(z, "%H:%M:%S")
z <- head(z,-1)
z2 <- do.call(paste, c(as.list(z), sep = ", "))

# add new column 'hours' to list with data.frames
#test_sampling$hour <- z2

# create string with trap/site names in order to add column with site/trap names --> replicate each row for all site/traps
traps <- do.call(paste, c(as.list(sites_2010$Trap), sep = ", "))


  for(i in 1:length(data_samp_clim)){
    
    # add new column 'hours' to list with data.frames
    data_samp_clim[[i]]$hour <- z2
    
    # split new column into separate rows and multiply each day-row into 24 day-hour-rows:
    data_samp_clim[[i]] <- 
      data_samp_clim[[i]] %>% 
      separate_longer_delim(hour, delim = ", ")
    
    # remove certain hours for start and end days (outside of sampling hours): 
    ## start days: remove hours < 12 pm
    ## end days: remove hours > 12 pm
    data_samp_clim[[i]]$hour <- ifelse((data_samp_clim[[i]]$`startend.spring` == 1 & data_samp_clim[[i]]$hour < "12:00:00"), NA, ifelse(
      (data_samp_clim[[i]]$`startend.spring` == 1 & data_samp_clim[[i]]$hour >= "12:00:00"), data_samp_clim[[i]]$hour, ifelse(
        (data_samp_clim[[i]]$`startend.spring` == 2 & data_samp_clim[[i]]$hour >= "12:00:00"), NA, ifelse(
          (data_samp_clim[[i]]$`startend.spring` == 2 & data_samp_clim[[i]]$hour < "12:00:00"), data_samp_clim[[i]]$hour, data_samp_clim[[i]]$hour))))
    # remove rows with NAs within column 'hour' (as these fall outside the sampling hours within start or end days)
    data_samp_clim[[i]] <- dplyr::filter(data_samp_clim[[i]],  !is.na(hour))
    
    data_samp_clim[[i]]$hour <- strptime(data_samp_clim[[i]]$hour, format = "%H:%M:%S") ## alternative: add time information to column 'date'
    
    # add new column 'Trap' to list with data.frames
    data_samp_clim[[i]]$Trap <- traps
    
    # split new column 'Trap' into separate rows and multiply each 'sampling time'-row into 95 sampling time -rows:
    data_samp_clim[[i]] <- 
      data_samp_clim[[i]] %>% 
      separate_longer_delim(Trap, delim = ", ")
    
    # add information on site coordinates and cell_id
    data_samp_clim[[i]] <- left_join(data_samp_clim[[i]], sites_id_2010, by = "Trap")
  
    }

# # split new column into separate rows and multiply each day-row into 24 day-hour-rows:
# test_sampling_l <- 
#   test_sampling %>% 
#   separate_longer_delim(hour, delim = ", ")

# # remove certain hours for start and end days (outside of sampling hours): 
# ## start days: remove hours < 12 pm
# ## end days: remove hours > 12 pm
# test_sampling_l$hour <- ifelse((test_sampling_l$`start/end` == 1 & test_sampling_l$hour < "12:00:00"), NA, ifelse(
#   (test_sampling_l$`start/end` == 1 & test_sampling_l$hour >= "12:00:00"), test_sampling_l$hour, ifelse(
#     (test_sampling_l$`start/end` == 2 & test_sampling_l$hour >= "12:00:00"), NA, ifelse(
#       (test_sampling_l$`start/end` == 2 & test_sampling_l$hour < "12:00:00"), test_sampling_l$hour, test_sampling_l$hour))))
# # remove rows with NAs within column 'hour' (as these fall outside the sampling hours within start or end days)
# test_sampling_l <- dplyr::filter(test_sampling_l,  !is.na(hour))
# 
# test_sampling_l$hour <- strptime(test_sampling_l$hour, format = "%H:%M:%S") ## alternative: add time information to column 'date'
# 
# # add information on site coordinates and cell_id
# test_sampling_l <- left_join(test_sampling_l, sites_id_2010, by = "Trap")


## 4.1 extract climate data from raster
# select proper .nc file and within there: proper raster 
# year + month --> select .nc file
# day + start/end (+ daytime) --> select raster within .nc file


# 4.1.1 Temperature data
# get name of .nc file of interest
# tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc
test_sampling_l$nc_temp <- NA
for(i in 1:nrow(test_sampling_l)){
  nc_name <- paste0("tas_1hr_HOSTRADA-v1-0_BE_gn_",format(test_sampling_l[i,]$date, "%Y"),format(test_sampling_l[i,]$date, "%m"),"0100-",
                    format(test_sampling_l[i,]$date, "%Y"),format(test_sampling_l[i,]$date, "%m"),days_in_month(test_sampling_l[i,]$date),"23.nc")
  test_sampling_l$nc_temp[i] <- nc_name
  }

# get number of raster file within the .nc file
## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)
# --> 5th day, 2am (14 o'clock) = (5-1)*24+14 = 110
# --> xth day, y o'clock = (x-1)*24 + y

test_sampling_l$raster_nr <- NA
for(i in 1:nrow(test_sampling_l)){
  # get day: 
  x <- as.numeric(format(test_sampling_l[i,]$date, "%d"))
  # get hour:
  y <- as.numeric(format(test_sampling_l[i,]$hour, "%H"))
  raster_nr <- (x-1)*24+1 + y
  test_sampling_l$raster_nr[i] <- raster_nr
}


# 4.1.2 Precipitation data

##
##
# to be done
##
##
##


# 4.2 extract climate data from raster
test_sampling_l$temp <- NA
for(i in 1:nrow(test_sampling_l)){
  link <- paste0("/hourly/hostrada/air_temperature_mean/",test_sampling_l$nc_temp[[i]])  #  5 MB
  file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
  rad <- readDWD(file) 
  test_sampling_l$temp[[i]] <- (terra::extract(raster, test_sampling_l$cell_id[[i]]))[[1]]
}



