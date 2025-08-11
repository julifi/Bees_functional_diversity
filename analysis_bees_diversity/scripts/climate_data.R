# BEFORE: run setting.R
getwd()
tempdir <- "analysis_bees_diversity/data/temp"
locdir <- "analysis_bees_diversity/data/temp"

## Load Libraries ############################################################# 
library(rdwd); library(RCurl); library(dplyr); library(readxl); library(terra); library(lubridate); 
library(tidyr); library(dwdradar); library(sf)
#rdwd::updateRdwd()

## 1. check available .nc-files on the dwd FTP server -------
# currently available files in a given folder:

# 1.1 Temperature data: 
## .nc files containing hourly temperature data for one month: 
## The name of the NetCDF file is formed as follows: parameter abbreviation}_{time resolution}_{process name version}_{variant}_{grid_info}_{time coverage}.nc
## (variant: BE - best estimate; grid_info: gn - native grid), e.g. tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc
## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)

rasterbase <- paste0(gridbase,"/hourly/hostrada")
ftp.files <- indexFTP("/air_temperature_mean", base=rasterbase, dir=tempdir())

# example data: 
link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025030100-2025033123.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
#plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=3)


# temperature reference raster
link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025040100-2025043023.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)
# set temperature reference raster: rad[[1]]
raster_ref <- rad[[1]]

# 1.2 Precipitation data
# # RADOLAN 'historical' contains missing data (missing layers within one raster stack = missing hours) --> NOT usable! instead use: RADOLAN 'reproduced' 

# RADOLAN reproduced: 2005-06-01 until today
# example data:
# format: BIN
link <- "hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz"  # 25 MB
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file, selection=1:2)
plotRadar(rad$dat, main=".binary RW", extent="rw", layer=1)
raster_ref_prec <- rad$dat[[1]]



## 2. load spatial data of TERENO sites --------------
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


# check whether lat.long are across the sampling time consistent
check<-aggregate(sites$lat,by = list(sites$Trap), function(x){length(unique(x))})
max(check[,2]) # yes they are
# --> choose lat.long for one example year, e.g. 2010

sites_2010 <- dplyr::filter(sites, Year == 2010)
sites_2010 <- sites_2010[c("Trap", "lon", "lat")]
sites_2010 <- sites_2010[,c(3,2,1)]
sites_2010$lat <- as.numeric(sites_2010$lat)
sites_2010$lon <- as.numeric(sites_2010$lon)
sites_2010 <- as.data.frame(sites_2010)

# convert site point data into SpatVector
sites_s <- vect(sites_2010, geom = c("lon", "lat"), crs="+proj=longlat")
plot(sites_s)


### 2.1 Temperature data
# use projection of temperature reference raster (ETRS89-extended / LCC Europe --> EPSG: 3034)
sites_proj <- project(sites_s, "EPSG:3034")
plot(sites_proj)

# extract grid cell IDs of sites for temperature data
sites_2010$cell_id_temp <- cellFromXY(raster_ref, crds(sites_proj))  

### 2.2 Precipitation data
#check projection of precipitation data: 
crs(raster_ref_prec) # --> no CRS given but information given in metadata on projection of the RADOLAN precipitation raster: "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0+a=6370040 +b=6370040 +units=m"
# assign CRS: 
raster_ref_prec <- projectRasterDWD(raster_ref_prec,
                                    proj = "radolan")
# check CRS
crs(raster_ref_prec) 
crs(sites_s)
#--> same CRS as sites_s

# extract grid cell IDs of sites for precipitation data
sites_2010$cell_id_prec <- cellFromXY(raster_ref_prec, crds(sites_s)) 
# note: temperature and precipitation raster have different cell IDs for the same Traps as though they have the same projection the raster extent is different 


## 3. choose relevant raster files (hourly resolution) on temp & precipitation from .nc files  -----------------------
# depending on: exact sampling days, daytime

# read prepared sampling data:
data_sampling <- readRDS("analysis_bees_diversity/data/sampling_days_siteyseason.RData")
data_samp_clim <- data_sampling

# load metadata:
meta <- read.csv("analysis_bees_diversity/data/meta_sampling_days_siteyseason.csv")

# add column with 'hour'; replicate each row 23times --> for 'hour' passing values from 00:00:00 to 23:00:00
# create vector with values from 00:00 to 23:00 
z <- seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 hour")
z <- format(z, "%H:%M:%S")
z <- head(z,-1)
z2 <- do.call(paste, c(as.list(z), sep = ", "))

# refine daytime
# load data on daytime hours: 
daytime_hours <- read_excel("analysis_bees_diversity/data/data_raw/daytime_hours.xlsx", sheet = "daytime_hours")
daytime_hours <- daytime_hours[c("month", "sunrise_hour", "sunset_hour")]
daytime_hours$sunrise_hour <- format(daytime_hours$sunrise_hour, "%H:%M:%S")
daytime_hours$sunset_hour <- format(daytime_hours$sunset_hour, "%H:%M:%S")

# split day-rows in data.frames of list 'data_samp_clim' into day-hour-rows
for(i in 1:length(data_samp_clim)){
  
  # add new column 'hours' to list with data.frames
  data_samp_clim[[i]]$hour <- z2
  
  # split new column into separate rows and multiply each day-row into 24 day-hour-rows:
  data_samp_clim[[i]] <- 
    data_samp_clim[[i]] %>% 
    separate_longer_delim(hour, delim = ", ")
  
  # remove hours for start and end days (outside of sampling hours): 
  ## start days: remove hours < 12 pm
  ## end days: remove hours > 12 pm
  
  # check if data.frame covers spring or summer season
  if(grepl('dates.spring', colnames(data_samp_clim[[i]]))[1] == TRUE){
    data_samp_clim[[i]]$hour <- ifelse((data_samp_clim[[i]]$`startend.spring` == 1 & data_samp_clim[[i]]$hour < "12:00:00"), NA, 
                                       ifelse((data_samp_clim[[i]]$`startend.spring` == 1 & data_samp_clim[[i]]$hour >= "12:00:00"), data_samp_clim[[i]]$hour, 
                                              ifelse((data_samp_clim[[i]]$`startend.spring` == 2 & data_samp_clim[[i]]$hour >= "12:00:00"), NA, 
                                                     ifelse((data_samp_clim[[i]]$`startend.spring` == 2 & data_samp_clim[[i]]$hour < "12:00:00"), data_samp_clim[[i]]$hour, data_samp_clim[[i]]$hour))))
    
    # get month to exclude non-daytime hours: 
    m <- as.numeric(format(data_samp_clim[[i]]$dates.spring, "%m"))
    
  }else{
    data_samp_clim[[i]]$hour <- ifelse((data_samp_clim[[i]]$`startend.summer` == 1 & data_samp_clim[[i]]$hour < "12:00:00"), NA, 
                                       ifelse((data_samp_clim[[i]]$`startend.summer` == 1 & data_samp_clim[[i]]$hour >= "12:00:00"), data_samp_clim[[i]]$hour, 
                                              ifelse((data_samp_clim[[i]]$`startend.summer` == 2 & data_samp_clim[[i]]$hour >= "12:00:00"), NA, 
                                                     ifelse((data_samp_clim[[i]]$`startend.summer` == 2 & data_samp_clim[[i]]$hour < "12:00:00"), data_samp_clim[[i]]$hour, data_samp_clim[[i]]$hour))))
    # get month to exclude non-daytime hours: 
    m <- as.numeric(format(data_samp_clim[[i]]$dates.summer, "%m"))
  }
  
  
  # exclude non-daytime hours
  data_samp_clim[[i]]$hour <- ifelse((m == 1 & data_samp_clim[[i]]$hour < "08:00:00"), NA, 
                                     ifelse((m == 1 & data_samp_clim[[i]]$hour > "16:00:00"), NA, 
                                            ifelse((m == 2 & data_samp_clim[[i]]$hour < "07:00:00"), NA, 
                                                   ifelse((m == 2 & data_samp_clim[[i]]$hour > "17:00:00"), NA,
                                                          ifelse((m == 3 & data_samp_clim[[i]]$hour < "06:00:00"), NA, 
                                                                 ifelse((m == 3 & data_samp_clim[[i]]$hour > "17:00:00"), NA,
                                                                        ifelse((m == 4 & data_samp_clim[[i]]$hour < "06:00:00"), NA, 
                                                                               ifelse((m == 4 & data_samp_clim[[i]]$hour > "19:00:00"), NA,
                                                                                      ifelse((m == 5 & data_samp_clim[[i]]$hour < "05:00:00"), NA, 
                                                                                             ifelse((m == 5 & data_samp_clim[[i]]$hour > "20:00:00"), NA,
                                                                                                    ifelse((m == 6 & data_samp_clim[[i]]$hour < "04:00:00"), NA, 
                                                                                                           ifelse((m == 6 & data_samp_clim[[i]]$hour > "21:00:00"), NA,
                                                                                                                  ifelse((m == 7 & data_samp_clim[[i]]$hour < "04:00:00"), NA, 
                                                                                                                         ifelse((m == 7 & data_samp_clim[[i]]$hour > "21:00:00"), NA,
                                                                                                                                ifelse((m == 8 & data_samp_clim[[i]]$hour < "05:00:00"), NA, 
                                                                                                                                       ifelse((m == 8 & data_samp_clim[[i]]$hour > "20:00:00"), NA,
                                                                                                                                              ifelse((m == 9 & data_samp_clim[[i]]$hour < "06:00:00"), NA, 
                                                                                                                                                     ifelse((m == 9 & data_samp_clim[[i]]$hour > "19:00:00"), NA,
                                                                                                                                                            ifelse((m == 10 & data_samp_clim[[i]]$hour < "07:00:00"), NA, 
                                                                                                                                                                   ifelse((m == 10 & data_samp_clim[[i]]$hour > "18:00:00"), NA,
                                                                                                                                                                          ifelse((m == 11 & data_samp_clim[[i]]$hour < "07:00:00"), NA, 
                                                                                                                                                                                 ifelse((m == 11 & data_samp_clim[[i]]$hour > "16:00:00"), NA,
                                                                                                                                                                                        ifelse((m == 12 & data_samp_clim[[i]]$hour < "07:00:00"), NA, 
                                                                                                                                                                                               ifelse((m == 12 & data_samp_clim[[i]]$hour > "16:00:00"), NA, data_samp_clim[[i]]$hour))))))))))))))))))))))))
  
  # remove rows with NAs within column 'hour' (as these fall outside the sampling hours within start or end days)
  data_samp_clim[[i]] <- dplyr::filter(data_samp_clim[[i]],  !is.na(hour))
  
  data_samp_clim[[i]]$hour <- as.POSIXct(paste0(data_samp_clim[[i]][,1], ' ',data_samp_clim[[i]]$hour), format="%Y-%m-%d %H:%M:%S")
}



## 4. Preparation for data extraction ------------------
### 4.1 Temperature data -------------
# select proper .nc file and within there: proper raster
# 1. year + month --> select .nc file 
# 2. day + start/end (+ daytime) --> select raster within .nc file

# 4.1.1 retrieve name of .nc file of interest: e.g. tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc
for(i in 1:length(data_samp_clim)){
  data_samp_clim[[i]]$nc_temp <- NA
  
  # check if data.frame covers spring or summer season
  if(grepl('dates.spring', colnames(data_samp_clim[[i]]))[1] == TRUE){
    nc_name <- paste0("tas_1hr_HOSTRADA-v1-0_BE_gn_",format(data_samp_clim[[i]]$dates.spring, "%Y"),format(data_samp_clim[[i]]$dates.spring, "%m"),"0100-",
                      format(data_samp_clim[[i]]$dates.spring, "%Y"),format(data_samp_clim[[i]]$dates.spring, "%m"),days_in_month(data_samp_clim[[i]]$dates.spring),"23.nc")
  }else{
    nc_name <- paste0("tas_1hr_HOSTRADA-v1-0_BE_gn_",format(data_samp_clim[[i]]$dates.summer, "%Y"),format(data_samp_clim[[i]]$dates.summer, "%m"),"0100-",
                      format(data_samp_clim[[i]]$dates.summer, "%Y"),format(data_samp_clim[[i]]$dates.summer, "%m"),days_in_month(data_samp_clim[[i]]$dates.summer),"23.nc")
  }
  
  data_samp_clim[[i]]$nc_temp <- nc_name
}

# 4.1.2 get number of raster file within the .nc file
## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)
# --> 5th day, 2am (14 o'clock) = (5-1)*24+14 = 110
# --> xth day, y o'clock = (x-1)*24 + y

for(i in 1:length(data_samp_clim)){
  data_samp_clim[[i]]$raster_nr <- NA
  
  # check if data.frame covers spring or summer season
  if(grepl('dates.spring', colnames(data_samp_clim[[i]]))[1] == TRUE){
    # get day: 
    x <- as.numeric(format(data_samp_clim[[i]]$dates.spring, "%d"))
    # get hour:
    y <- as.numeric(format(data_samp_clim[[i]]$hour, "%H"))
  }else{
    # get day: 
    x <- as.numeric(format(data_samp_clim[[i]]$dates.summer, "%d"))
    # get hour:
    y <- as.numeric(format(data_samp_clim[[i]]$hour, "%H"))
  }
  raster_nr <- (x-1)*24+1 + y
  data_samp_clim[[i]]$raster_nr<- raster_nr
}

### 4.2 Precipitation data -------------
# select proper .tar.gz file and within there: proper raster
# 1. year + month --> select .tar.gz file 
# 2. day + start/end (+ daytime) --> select raster within .tar.gz file

# 4.2.1 get name of .tar file of interest --> select proper folder (year) and file (month) --> defines 'link'
for(i in 1:length(data_samp_clim)){
  data_samp_clim[[i]]$link_prec <- NA
  
  # check if data.frame covers spring or summer season
  if(grepl('dates.spring', colnames(data_samp_clim[[i]]))[1] == TRUE){
    link_prec <- paste0(format(data_samp_clim[[i]]$dates.spring, "%Y"),"/RW2017.002_",format(data_samp_clim[[i]]$dates.spring, "%Y"),format(data_samp_clim[[i]]$dates.spring, "%m"),".tar.gz")
  }else{
    link_prec <- paste0(format(data_samp_clim[[i]]$dates.summer, "%Y"),"/RW2017.002_",format(data_samp_clim[[i]]$dates.summer, "%Y"),format(data_samp_clim[[i]]$dates.summer, "%m"),".tar.gz")
  }
  data_samp_clim[[i]]$link_prec <- link_prec
}

# 4.2.2 number of raster file within the .tar file
## per .tar file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)

# --> same raster number as for temperature data (.nc files) --> already exists



# 5. Extraction of data from raster files -------
# 5.1. Create/ load master data.frame -----

# add cell ID of temperature and precipitation raster to meta-data
# cell ID temperature raster:
meta$cellID_temp<-sites_2010$cell_id_temp[match(meta$LocTrap, sites_2010$Trap)]
# cell ID precipitation raster:
meta$cellID_prec<-sites_2010$cell_id_prec[match(meta$LocTrap, sites_2010$Trap)]


# prepare master data.frame containing all day-hour-site combinations & information on the .nc-file (temperature raster stack) , .tar-file (precipitation raster stack) and respective raster layer
# if already done --> load data in line 294
all.dat<-c()
for(i in 1:length(data_samp_clim)){
  x<-data_samp_clim[[i]]; x<-cbind(x[5:8], 
                                   cellID_temp = rep(meta$cellID_temp[i], nrow(x)), 
                                   cellID_prec = rep(meta$cellID_prec[i], nrow(x)), 
                                   trap = rep(meta$LocTrap[i], nrow(x))) 
  all.dat<-rbind(all.dat, x)
}

# save master data.frame "all.dat"
write.csv(all.dat,"analysis_bees_diversity/data/all.dat.csv", row.names = FALSE)

# load master data.frame "all.dat"
all.dat <- read.csv("analysis_bees_diversity/data/all.dat.csv")


# 5.2 temperature data ----- 
# now let's run the extraction procedure --> if already done: proceed in line 320
unique.nc<-unique(all.dat$nc_temp)
extracted.data.temp<-c()

for(i in 1:length(unique.nc)){
  print(i)
  link <- paste0("/hourly/hostrada/air_temperature_mean/", unique.nc[i])
  file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
  rad <- readDWD(file)
  
  selection<-which(all.dat$nc_temp==unique.nc[i])
  unique.layer<-unique(all.dat$raster_nr[selection])
  for (k in 1:length(unique.layer)){
    print(k)
    layer <- rad[[unique.layer[k]]]
    
    selection2<- which(all.dat$nc_temp==unique.nc[i] & all.dat$raster_nr==unique.layer[k])
    unique.cells<-unique(all.dat$cellID_temp[selection2])
    extracted<- data.frame(temp = (terra::extract(layer, unique.cells)),
                           cellID_temp = unique.cells,
                           raster_nr = rep(unique.layer[k], length(unique.cells)),
                           nc_temp = rep(unique.nc[i], length(unique.cells)))
    colnames(extracted)[1]<-c('temp')
    extracted.data.temp<-rbind(extracted.data.temp, extracted)
  }
}
# save data on extracted temp values
write.csv(extracted.data.temp,"analysis_bees_diversity/data/extracted.data.temp_FULL.csv", row.names = FALSE)


# 5.3 precipitation data -----
unique.tar<-unique(all.dat$link_prec)

# for July 2012 (2012/RW-201207.tar) the data in the binary format within 'RADOLAN reproduced 2017_002' (radolan/reproc/2017_002/bin/2012) is incomplete
# (3 hour missing: 1207061350 (July 6th 13:50); 1207140650 (July 14th 06:50); 1207140750 (July 14th 07:50)) 
# the missing cdata is included in 'RADOLAN reproduced 2016_003' (radolan/reproc/2016_003/bin/2012)
# --> in the following loop July 2012 is skipped and the loop has been split into "i in 1:10" (May 2010 to June 2012) and "i in 12:length(unique.tar)" (August 2012 to September 2021) 
# the data for July 2012 is subsequently extracted from 'RADOLAN reproduced 2016_003'

# 5.3.1 precipitation data: May 2010 - June 2012
extracted.data.prec_1<-c()
for(i in 1:10){
  print(i)
  link <- paste0("hourly/radolan/reproc/2017_002/bin/", unique.tar[i])
  file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE) 
  rad <- readDWD(file, dividebyten=FALSE)
  
  selection<-which(all.dat$link_prec==unique.tar[i])
  unique.layer<-unique(all.dat$raster_nr[selection])
  for (k in 1:length(unique.layer)){
    print(k)
    layer <- rad$dat[[unique.layer[k]]]
    # assign CRS: 
    layer <- projectRasterDWD(layer,proj = "radolan")
    
    selection2<- which(all.dat$link_prec==unique.tar[i] & all.dat$raster_nr==unique.layer[k])
    unique.cells<-unique(all.dat$cellID_prec[selection2])
    extracted<- data.frame(prec = (terra::extract(layer, unique.cells)),
                           cellID_prec = unique.cells,
                           raster_nr = rep(unique.layer[k], length(unique.cells)),
                           link_prec = rep(unique.tar[i], length(unique.cells)))
    colnames(extracted)[1]<-c('prec')
    extracted.data.prec_1<-rbind(extracted.data.prec_1, extracted)
  }
}

# save data on extracted prec values
write.csv(extracted.data.prec_1,"analysis_bees_diversity/data/extracted.data.prec_2010_05_to_2012_06.csv", row.names = FALSE)


# 5.3.2 precipitation data: August 2012 - September 2021
extracted.data.prec_2<-c()
#for(i in 12:length(unique.tar)){
#for(i in 28:length(unique.tar)){
for(i in 43:length(unique.tar)){
  print(i)
  link <- paste0("hourly/radolan/reproc/2017_002/bin/", unique.tar[i])
  file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE) 
  rad <- readDWD(file, dividebyten=FALSE)
  
  selection<-which(all.dat$link_prec==unique.tar[i])
  unique.layer<-unique(all.dat$raster_nr[selection])
  for (k in 1:length(unique.layer)){
    print(k)
    layer <- rad$dat[[unique.layer[k]]]
    # assign CRS: 
    layer <- projectRasterDWD(layer,proj = "radolan")
    
    selection2<- which(all.dat$link_prec==unique.tar[i] & all.dat$raster_nr==unique.layer[k])
    unique.cells<-unique(all.dat$cellID_prec[selection2])
    extracted<- data.frame(prec = (terra::extract(layer, unique.cells)),
                           cellID_prec = unique.cells,
                           raster_nr = rep(unique.layer[k], length(unique.cells)),
                           link_prec = rep(unique.tar[i], length(unique.cells)))
    colnames(extracted)[1]<-c('prec')
    extracted.data.prec_2<-rbind(extracted.data.prec_2, extracted)
  }
}

# save data on extracted prec values
write.csv(extracted.data.prec_2,"analysis_bees_diversity/data/extracted.data.prec_2012_08_to_2021_09.csv", row.names = FALSE)

# 5.3.3 precipitation data: July 2012
link <- "hourly/radolan/reproc/2016_003/bin/2012/RW2016.003_201207.tar.gz"  # 25 MB
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file, dividebyten=FALSE)

extracted.data.prec_3<-c()

selection<-which(all.dat$link_prec=="2012/RW2017.002_201207.tar.gz")
unique.layer<-unique(all.dat$raster_nr[selection])
for (k in 1:length(unique.layer)){
  print(k)
  layer <- rad$dat[[unique.layer[k]]]
  # assign CRS: 
  layer <- projectRasterDWD(layer,proj = "radolan")
  
  selection2<- which(all.dat$link_prec=="2012/RW2017.002_201207.tar.gz" & all.dat$raster_nr==unique.layer[k])
  unique.cells<-unique(all.dat$cellID_prec[selection2])
  extracted<- data.frame(prec = (terra::extract(layer, unique.cells)),
                         cellID_prec = unique.cells,
                         raster_nr = rep(unique.layer[k], length(unique.cells)),
                         link_prec = rep("2012/RW2017.002_201207.tar.gz", length(unique.cells)))
  colnames(extracted)[1]<-c('prec')
  extracted.data.prec_3<-rbind(extracted.data.prec_3, extracted)
}

write.csv(extracted.data.prec_3,"analysis_bees_diversity/data/extracted.data.prec_2021_07.csv", row.names = FALSE)


# load precipitation data: 
# May 2010 - June 2012
extracted.data.prec_1 <- read.csv("analysis_bees_diversity/data/extracted.data.prec_2010_05_to_2012_06.csv")

# August 2012 - September 2021
extracted.data.prec_2 <- read.csv("analysis_bees_diversity/data/extracted.data.prec_2012_08_to_2021_09.csv")

# July 2012
extracted.data.prec_3 <- read.csv("analysis_bees_diversity/data/extracted.data.prec_2021_07.csv")

# bind data.frames
extracted.data.prec <- rbind(extracted.data.prec_1, extracted.data.prec_2, extracted.data.prec_3)
write.csv(extracted.data.prec,"analysis_bees_diversity/data/extracted.data.prec_FULL.csv", row.names = FALSE)


# 5.4 Add temperature & precipitation data to master data.frame -----
# load temperature data: 
extracted.data.temp <- read.csv("analysis_bees_diversity/data/extracted.data.temp_FULL.csv")
# load precipitation data: 
extracted.data.prec <- read.csv("analysis_bees_diversity/data/extracted.data.prec_FULL.csv")

# check for duplicates:
extracted.data.temp[duplicated(extracted.data.temp), ] # --> 0 duplicates
extracted.data.prec[duplicated(extracted.data.prec), ] # --> 0 duplicates


# merge all.dat with data on temperature and precipitation
all.dat.temp <- left_join(all.dat, extracted.data.temp, by = c("raster_nr", "nc_temp", "cellID_temp"), copy=FALSE)
all.dat.prec <- left_join(all.dat, extracted.data.prec, by = c("raster_nr", "link_prec", "cellID_prec"), copy=FALSE)

all.dat.temp.prec <- left_join(all.dat.temp, extracted.data.prec, by = c("raster_nr", "link_prec", "cellID_prec"), copy=FALSE)


temp.NA <- which(is.na(all.dat.temp$temp))
prec.NA <- which(is.na(all.dat.prec$prec))

# test NA values in extracted.data.prec:
## e.g.:
#layer: 270
#link: 2010/RW2017.002_201005.tar.gz
link <- "hourly/radolan/reproc/2017_002/bin/2010/RW2017.002_201005.tar.gz"  # 25 MB
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file)
plotRadar(rad$dat, main=".binary RW", extent="rw", layer=269)
plotRadar(rad$dat, main=".binary RW", extent="rw", layer=270)
plotRadar(rad$dat, main=".binary RW", extent="rw", layer=271)
# --> no data available for respective layer (270)





### next steps:
#(ii) combine extracted.data.temp and extracted.data.prec 
#(iii) back-transform the data so it is read to use 
#(iv) training data: run the selection procedure of best model constants (best settings for suitability approach)
#(v) testing data: compare the two modelling approaches (exposure days vs. suitability scores), accounting 
#     for co-lineraity, non-linearty and different scales
# (v) look as seasonality effects



# 6. compute suitability scores for wild-bee pollination for each sampling interval  -------

# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt <- seq(15,27, length=10) # optimal temperature - highest activity
t.max <- seq(25,45, length=10) # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=10) # defines the shape of the sigmodid shape of bee activity below t.opt
constants.grid<- expand.grid(t.opt, t.max, sigma)
names(constants.grid) <- c("t.opt", "t.max", "sigma")
# account for the fact that max temp needs to be at least 1 degree above opt. temperature
constants.grid<- constants.grid[which(constants.grid$t.opt<=constants.grid$t.max+1),]
rm(t.opt, t.max, sigma)

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data)){
  placeholder<-  input.data[[i]] # we extract the climate data of a given sampling period
  # for now, we assume made-up data, we can delete this later... 
  placeholder<- data.frame(temp=seq(10,35, length=100), rainfall = sample(c(0,0,0,0,10),100, replace = T))
  # prepare output data-frame for a given sampling period
  output.period<-c()
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$rainfall==0 & placeholder$temp <= constants.grid$t.opt[j])
    above.opt<-which(placeholder$rainfall==0 & placeholder$temp > constants.grid$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid$t.opt[j])/
                                              (2*constants.grid$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid$t.opt[j])/
                                           (constants.grid$t.opt[j]- constants.grid$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
}
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder)

### 7. load in predictors and response variables and prepare them  -------

# load in data
meta.trapyearseason<- read.csv('analysis_bees_diversity/data/meta.trapyearseason.csv')
site.env.data<-read.csv('analysis_bees_diversity/data/env_data_ecosystematlas_elevation.csv', 
                        dec = '.', sep = ',')

# match site.data with meta-data
matching<-match(meta.trapyearseason$site, site.env.data$TRAP)
site.env.data<-site.env.data[matching,]
site.env.data<-site.env.data[,-which(colnames(site.env.data)=='YEAR')]

# compute additional predictors 
# habitat richness: should probably only be calculated from semi-natural habitats
library(vegan)
seminat<- colnames(site.env.data)[c(5,8:12,16)]
meta.trapyearseason$hab.div<-apply(site.env.data[ ,c(5,8:12,16)],1, function(x){length(which(x>0))})
meta.trapyearseason$hab.even<-apply(site.env.data[ ,c(5,8:12,16)],1, function(x){
  diversity(x, index = 'shannon')/log(specnumber(x))})
meta.trapyearseason$hab.proportion<-apply(site.env.data[ ,c(5,8:12,16)],1, function(x){sum(x)})

rm(seminat)

# add the elevation data to the meta data
meta.trapyearseason$elevation_mean_400m<-site.env.data$elevation_mean_400m
meta.trapyearseason$elevation_range_400m<-site.env.data$elevation_range_400m

### 7. start modelling the impact of exposure time on abundance and richness  -------
library(lme4); library(lmerTest)
abund.1<-lmer(data= meta.trapyearseason, 
              abundance~ exposure_days + season + exposure_days:season + 
                elevation_mean_400m + elevation_range_400m + hab.even + hab.proportion + hab.div+ year +
                (1|location)+(1|year)+(1|site), REML = T)
abund.1<-lmer(data= meta.trapyearseason, 
              abundance~ exposure_days + season + exposure_days:season + 
                elevation_mean_400m + elevation_range_400m + year +
                (1|location)+(1|year)+(1|site), REML = T)
summary(abund.1)


rich.1<-lmer(data= meta.trapyearseason, 
             richness~ exposure_days + season + exposure_days:season + 
               elevation_mean_400m + elevation_range_400m + hab.even + hab.proportion + hab.div+ year +
               (1|location)+(1|year)+(1|site), REML = T)
summary(rich.1)

library(performance) 
r2(rich.1)
r2(abund.1)


library(ggplot2)

# look at abundance trend over time... 
ggplot(data= meta.trapyearseason, aes(x=year,))

colnames(meta.trapyearseason)
str(meta.trapyearseason)
# IDEA: water (rivers) might be a good additional predictor - we could explore this at a later point in time

### 8. start modelling the grid for the suitability score and identify the best solution  -------

# run the regression analysis for all constant combinations...
for(i in 1:ncol(output)){print(i)
  meta.trapyearseason$suitability<-output[,i]
}




# next steps:
# - load in the other predictors and the random effect variables. 
# - establish the regression model structure and let it run for all data combinations; play around with
#       non-linearities in the process
# - choose the best model and compare it with a model that relies on sampling days only...

# to do: 
# i) implement the switch function that defines whether an hour was suitable for wild-bee pollination
# ii) create a grid that contains different constants defining the switch function
# iii) sum the scores up for each hour within a sampling season for each combination of constants in the grid
#       this results in a suitability score for each sampling period
# iv) create regression models that evaluates whether the suitability score is a better predictor of abundance
#       and richness than simply the number of exposure days;
# point five would be tricky - we have to account for 
#     - random effects (site, location, year)
#     - other fixed effects (year, elevation, habitat diversity stuff??)
#     - things we are truly interested in (season, suitability score and their interaction) 
# test new