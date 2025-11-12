# BEFORE: run setting.R
getwd()
tempdir <- "analysis_bees_diversity/data/temp"
locdir <- "analysis_bees_diversity/data/temp"

## Load Libraries ############################################################# 
library(rdwd); library(RCurl); library(dplyr); library(readxl); library(terra); library(lubridate); 
library(tidyr); library(dwdradar); library(sf)
#rdwd::updateRdwd()

#### 1. link sampling locations to position in rasters of temperature and precipitation data ####

# 1.1 Temperature data: create a reference raster
## .nc files containing hourly temperature data for one month: 
## The name of the NetCDF file is formed as follows: parameter abbreviation}_{time resolution}_{process name version}_{variant}_{grid_info}_{time coverage}.nc
## (variant: BE - best estimate; grid_info: gn - native grid), e.g. tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc
## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)

# temperature reference raster - is necessary for linking cells in the raster to location of sampling points
link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025040100-2025043023.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)
# set temperature reference raster: rad[[1]]
raster_ref <- rad[[1]]

# 1.2 Precipitation data: create a reference raster
# we need a different reference raster for percipitation as the data structure is here different
# # RADOLAN 'historical' contains missing data (missing layers within one raster stack = missing hours) --> NOT usable! instead use: RADOLAN 'reproduced' 

# RADOLAN reproduced: 2005-06-01 until today
# example data:
# format: BIN
link <- "hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz"  # 25 MB
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file, selection=1:2)
#plotRadar(rad$dat, main=".binary RW", extent="rw", layer=1)
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

# load metadata:
meta <- read.csv("analysis_bees_diversity/data/meta_sampling_days_siteyseason.csv")

# add cell ID of temperature and precipitation raster to meta-data
# cell ID temperature raster:
meta$cellID_temp<-sites_2010$cell_id_temp[match(meta$LocTrap, sites_2010$Trap)]
# cell ID precipitation raster:
meta$cellID_prec<-sites_2010$cell_id_prec[match(meta$LocTrap, sites_2010$Trap)]

# clean up
rm(sites, sites_2010, sites_proj, sites_s, raster_ref, raster_ref_prec, rad, check)

# there is a data-entry mistake within the meta data file:
meta$LocTrap[which(meta$LocTrap=='FBG02*')]<-'FBG02'


############ 3. Change resolution of sampling seasons from daily to hourly #################
# depending on: light availability (day time) and start/ end of sampling period (assumes a establishment and abolisment of traps at noon)

# read prepared sampling data:
sampling.days <- readRDS("analysis_bees_diversity/data/sampling_days_siteyseason.RData")
data_samp_clim <- sampling.days


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
                                                     ifelse((data_samp_clim[[i]]$`startend.spring` == 2 & data_samp_clim[[i]]$hour < "12:00:00"), 
                                                            data_samp_clim[[i]]$hour, data_samp_clim[[i]]$hour))))
    
    # get month to exclude non-daytime hours: 
    m <- as.numeric(format(data_samp_clim[[i]]$dates.spring, "%m"))
    
  }else{
    data_samp_clim[[i]]$hour <- ifelse((data_samp_clim[[i]]$`startend.summer` == 1 & data_samp_clim[[i]]$hour < "12:00:00"), NA, 
                                       ifelse((data_samp_clim[[i]]$`startend.summer` == 1 & data_samp_clim[[i]]$hour >= "12:00:00"), data_samp_clim[[i]]$hour, 
                                              ifelse((data_samp_clim[[i]]$`startend.summer` == 2 & data_samp_clim[[i]]$hour >= "12:00:00"), NA, 
                                                     ifelse((data_samp_clim[[i]]$`startend.summer` == 2 & data_samp_clim[[i]]$hour < "12:00:00"), data_samp_clim[[i]]$hour,
                                                            data_samp_clim[[i]]$hour))))
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
### 4.1 Temperature data: add information which T file matches with sampling hours to the data_samp_clim list  -----------
# select proper .nc file and within there: proper raster
# 1. year + month --> select .nc file 
# 2. day + start/end (+ daytime) --> select raster within .nc file

# 4.1.1 retrieve name of .nc file of interest: e.g. tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc
for(i in 1:length(data_samp_clim)){
  data_samp_clim[[i]]$nc_temp <- NA
  
  # check if data.frame covers spring or summer season
  if(grepl('dates.spring', colnames(data_samp_clim[[i]]))[1] == TRUE){
    nc_name <- paste0("tas_1hr_HOSTRADA-v1-0_BE_gn_",format(data_samp_clim[[i]]$dates.spring, "%Y"),
                      format(data_samp_clim[[i]]$dates.spring, "%m"),"0100-", format(data_samp_clim[[i]]$dates.spring, "%Y"),
                      format(data_samp_clim[[i]]$dates.spring, "%m"),days_in_month(data_samp_clim[[i]]$dates.spring),"23.nc")
  }else{
    nc_name <- paste0("tas_1hr_HOSTRADA-v1-0_BE_gn_",format(data_samp_clim[[i]]$dates.summer, "%Y"),
                      format(data_samp_clim[[i]]$dates.summer, "%m"),"0100-", format(data_samp_clim[[i]]$dates.summer, "%Y"),
                      format(data_samp_clim[[i]]$dates.summer, "%m"),days_in_month(data_samp_clim[[i]]$dates.summer),"23.nc")
  }
  data_samp_clim[[i]]$nc_temp <- nc_name
}

# 4.1.2 add information which T layer matches with sampling hours to the data_samp_clim list 
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
# add information which precipitation file matches with sampling hours to the data_samp_clim list
# layer structure of precipitation and temperature data is identical and hence, we can use the layer info of T
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
rm(x,y,raster_nr, nc_name, link_prec, daytime_hours)


# save "data_samp_clim"
write.csv(data_samp_clim,"analysis_bees_diversity/data/data_weather/all.dat.csv", row.names = FALSE)


# 5. Extraction of data from raster files -------
# 5.1. Create/ load master data.frame -----

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

# check for double entries in the all data - there should be none
check<-table(paste0(all.dat$hour, all.dat$trap))
check<-as.data.frame(check)
dublicates<-which(check$Freq>1)
rm(check, dublicates)

# save master data.frame "all.dat"
write.csv(all.dat,"analysis_bees_diversity/data/data_weather/all.dat.csv", row.names = FALSE)

# load master data.frame "all.dat"
all.dat <- read.csv("analysis_bees_diversity/data/data_weather/all.dat.csv")

# 5.2 temperature data ----- 
# now let's run the extraction procedure --> if already done: proceed in line 320
unique.nc<-unique(all.dat$nc_temp)
extracted.data.temp<-c()

for(i in 22:length(unique.nc)){
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
write.csv(extracted.data.temp,"analysis_bees_diversity/data/data_weather/extracted.data.temp_FULL.csv", row.names = FALSE)

# 5.3 precipitation data -----
# for July 2012 (2012/RW-201207.tar) the data in the binary format within 'RADOLAN reproduced 2017_002' (radolan/reproc/2017_002/bin/2012) is incomplete
# (3 hour missing: 1207061350 (July 6th 13:50); 1207140650 (July 14th 06:50); 1207140750 (July 14th 07:50)) 
# the missing cdata is included in 'RADOLAN reproduced 2016_003' (radolan/reproc/2016_003/bin/2012)
# --> in the following loop July 2012 is skipped and the loop has been split into "i in 1:10" (May 2010 to June 2012) and "i in 12:length(unique.tar)" (August 2012 to September 2021) 
# the data for July 2012 is subsequently extracted from 'RADOLAN reproduced 2016_003'

unique.tar<-unique(all.dat$link_prec)

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
write.csv(extracted.data.prec_1,"analysis_bees_diversity/data/data_weather/extracted.data.prec_2010_05_to_2012_06.csv", row.names = FALSE)


# 5.3.2 precipitation data: August 2012 - September 2021
extracted.data.prec_2<-c()
for(i in 40:length(unique.tar)){
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
write.csv(extracted.data.prec_2,"analysis_bees_diversity/data/data_weather/extracted.data.prec_2012_08_to_2021_09.csv", row.names = FALSE)

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

write.csv(extracted.data.prec_3,"analysis_bees_diversity/data/data_weather/extracted.data.prec_2021_07.csv", row.names = FALSE)


# load precipitation data: 
# May 2010 - June 2012
extracted.data.prec_1 <- read.csv("analysis_bees_diversity/data/data_weather/extracted.data.prec_2010_05_to_2012_06.csv")

# August 2012 - September 2021
extracted.data.prec_2 <- read.csv("analysis_bees_diversity/data/data_weather/extracted.data.prec_2012_08_to_2021_09.csv")

# July 2012
extracted.data.prec_3 <- read.csv("analysis_bees_diversity/data/data_weather/extracted.data.prec_2021_07.csv")

# bind data.frames
extracted.data.prec <- rbind(extracted.data.prec_1, extracted.data.prec_2, extracted.data.prec_3)
write.csv(extracted.data.prec,"analysis_bees_diversity/data/data_weather/extracted.data.prec_FULL.csv", row.names = FALSE)



# 5.4 Add temperature & precipitation data to master data.frame -----
# load temperature data: 
# note: the extracted.data.temp has a different number of rows because some traps are in the same raster cell.
extracted.data.temp <- read.csv("analysis_bees_diversity/data/data_weather/extracted.data.temp_FULL.csv")

# load precipitation data: 
# note: the extracted.data.prec has a different number of rows because some traps are in the same raster cell.
extracted.data.prec <- read.csv("analysis_bees_diversity/data/data_weather/extracted.data.prec_FULL.csv")

# check for duplicates:
extracted.data.temp[duplicated(extracted.data.temp), ] # --> 0 duplicates
extracted.data.prec[duplicated(extracted.data.prec), ] # --> 0 duplicates

# merge all.dat with data on temperature and precipitation
all.dat.temp <- left_join(all.dat, extracted.data.temp, by = c("raster_nr", "nc_temp", "cellID_temp"), copy=FALSE)
all.dat.prec <- left_join(all.dat, extracted.data.prec, by = c("raster_nr", "link_prec", "cellID_prec"), copy=FALSE)
all.dat.temp.prec <- left_join(all.dat.temp, extracted.data.prec, by = c("raster_nr", "link_prec", "cellID_prec"), copy=FALSE)




# 5.5 Quality control of the data -----
# check for NAs
temp.NA <- which(is.na(all.dat.temp.prec$temp)) # nice - the T data is complete
prec.NA <- which(is.na(all.dat.temp.prec$prec)) # here we have a couple of NAs; these are not because of data entry mistakes

# but reflect true missing data: 
# # test NA values in extracted.data.prec:
# ## e.g.:
# #layer: 270
# #link: 2010/RW2017.002_201005.tar.gz
# link <- "hourly/radolan/reproc/2017_002/bin/2010/RW2017.002_201005.tar.gz"  # 25 MB
# file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
# rad <- readDWD(file)
# plotRadar(rad$dat, main=".binary RW", extent="rw", layer=269)
# plotRadar(rad$dat, main=".binary RW", extent="rw", layer=270)
# plotRadar(rad$dat, main=".binary RW", extent="rw", layer=271)
# rad$dat[[269]]
# rad$dat[[270]]
# rad$dat[[271]]
# # --> no data available for respective layer (270)


# add landscape ID to all.dat.prec 
all.dat.temp.prec$landscape<- substr(all.dat.temp.prec$trap,0,3)

# create an object that contains all missing data 
missing<-all.dat.temp.prec[which(is.na(all.dat.prec$prec)), ]
missing$unique.ID<-paste0(missing$hour, missing$landscape)

# count for each hour-landscape combination the number of missing data points
no.unique.ID<-as.data.frame(table(missing$unique.ID))
colnames(no.unique.ID)<-c('no.unique.ID','freq_NAs')

# create an object that contains all data for which at least one trap per landscape is missing 
no.unique.ID.all.dat<-paste0(all.dat.temp.prec$hour, all.dat.temp.prec$landscape)
x<-all.dat.temp.prec[which(is.element(no.unique.ID.all.dat, no.unique.ID$no.unique.ID)),]
x$unique.ID<-paste0(x$hour, x$landscape)

# compute how many data points are available in total per hour-landscape combination
y<-as.data.frame(table(x$unique.ID))
colnames(y)<-c('no.unique.ID','freq_data_landscape')

# add this info to the missing data information
no.unique.ID$freq_data_landscape<-y$freq_data_landscape

# get the hour and landscape info in the data frame
no.unique.ID<-cbind(no.unique.ID, missing[match(no.unique.ID$no.unique.ID, missing$unique.ID),c(1,4,6,10)])

# count how many missing data there are for each hour
x<-aggregate(no.unique.ID$freq_NAs, by=list(no.unique.ID$hour), function(x){sum(x)})

no.unique.ID$freq_missing_whole_data<-x$x[match(no.unique.ID$hour,x$Group.1)]

# count how many data points there are for each hour that contains at least one missing data entry
x<-all.dat.temp.prec[which(is.element(all.dat.temp.prec$hour, no.unique.ID$hour)),]
y<-as.data.frame(table(x$hour))
colnames(y)<-c('unique_hr','freq_all_data')

no.unique.ID$freq_all_data<-y$freq_all_data[match(no.unique.ID$hour,y$unique_hr)]
# so now we have a data frame that tells us how many local, how many regional and how many super-regional
# problems we have.

# inter-landscape extrapolation impossible
sum(no.unique.ID$freq_NAs[which(no.unique.ID$freq_all_data == no.unique.ID$freq_missing_whole_data)])

x<-no.unique.ID[which(no.unique.ID$freq_all_data == no.unique.ID$freq_missing_whole_data),]

# intra-landscape extrapolation possible
sum(no.unique.ID$freq_NAs)-sum(no.unique.ID$freq_NAs[which(no.unique.ID$freq_NAs == no.unique.ID$freq_data_landscape)])

# 
x<-no.unique.ID[which(no.unique.ID$freq_NAs == no.unique.ID$freq_data_landscape & 
                        no.unique.ID$freq_all_data != no.unique.ID$freq_missing_whole_data),]


## 5.5.2 Bounding box --------------
### Precipitation data: create reference raster
link <- "hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz"
file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE)
rad <- readDWD(file, selection=1:2)
raster_ref_prec <- rad$dat[[1]]
#check projection of precipitation data: 
crs(raster_ref_prec) # --> no CRS given but information given in metadata on projection of the RADOLAN precipitation raster: "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0+a=6370040 +b=6370040 +units=m"
# assign CRS: 
raster_ref_prec <- projectRasterDWD(raster_ref_prec,
                                    proj = "radolan")

# project raster in order to be able to calculate metric distances
raster_ref_prec_proj <- project(raster_ref_prec, "EPSG:3857")

### load spatial data of TERENO sites 
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
# extract grid cell IDs of sites for precipitation data
sites_2010$cell_id_sites <- cellFromXY(raster_ref_prec, crds(sites_s)) 



# convert site point data into sf object
sites_sf <- st_as_sf(sites_2010, 
                     coords = c("lon", "lat"), 
                     crs="+proj=longlat")
# project site point data (in order to later calculate a metric distance)
#sites_proj <- st_transform(sites_sf, crs = 3857) # using EPSG:3857 WGS 84 / Pseudo-Mercator -- Spherical Mercator

#create buffer of 30 km around trap sites (30 000 m)
buffer_dist <- 30000
sites_buffer <- st_buffer(sites_sf, dist = buffer_dist)
# create bounding box:
bbox <- st_bbox(sites_buffer)
# transform bbox into polygon and then into sf object: 
bbox_poly <- st_as_sfc(bbox)
bbox_sf <- st_sf(geometry = bbox_poly)

# create list of cell-IDs of precipitation raster which overlap with bounding box of sites + buffers
cell_id_bbox <- cells(raster_ref_prec, vect(bbox_sf))
cell_ids <- as.vector(unlist(cell_id_bbox[,2]))

## 5.6.2 Create dataframe with distances between site points and cells within the bounding box of sites + buffers -------------- 
# extract cell center from list of cell ids and from site cells:
coords_cell_id <- terra::xyFromCell(raster_ref_prec, cell_ids)
# transform into sf object: 
cell_id_sf <- st_as_sf(
  data.frame(cell_id = cell_ids, coords_cell_id),
  coords = c("x", "y"),
  crs = st_crs(sites_sf)
)

#  create matrix with distances between site points and cell_id center points
dist_matrix <- st_distance(cell_sf, sites_sf)  # Ergebnis: units-Matrix (m)

# transform matrix into dataframe
dist_df <- as.data.frame(dist_matrix)
colnames(dist_df) <- sites_sf$cell_id_sites
dist_df$cell_id <- cell_sf$cell_id
dist_df <- dist_df |> relocate(cell_id)




## 5.5.3 Loop through precipitation raster and extract precipitation value of the raster cell which is closest to the site point 

# replacement steps:
# A) define bounding box
# B) determine, which raster cells are in the bounding box and create a data frame with their ID
# C) Create a data frame that contains 96 columns (for each trap one) and the distance between all raster points and trap-raster cells
#    (number of rows is equal the number of raster cells in the bounding box)
# D) create a loop for each unique months (data file) that needs to be loaded. (only select months for which data is missing)
# within the loop get a second for the unique hours within the data frame where we have missing data
# E) Then create a third loop for each of the traps that has missing data for that hour.
# F) In that loop look for the closets raster cell(s) in the bounding box that still contains data
# G) add this value to the 'missing' data-frame at the right position



# dataframe with distances between site points and raster cells: dist_df 
# dataframe with missing data: missing

#remove unneccessary columns from missing
missing[,c("nc_temp", "cellID_temp", "temp")] <- NULL

unique.tar<-unique(missing$link_prec)

# extract missing precipitation data
missing.data.prec<-c()
for(i in 42:length(unique.tar)){
  print(i)
  link <- paste0("hourly/radolan/reproc/2017_002/bin/", unique.tar[i])
  file <- dataDWD(link, base=gridbase, dir=tempdir, joinbf=TRUE, read=FALSE) 
  rad <- readDWD(file, dividebyten=FALSE)
  
  selection<-which(missing$link_prec==unique.tar[i])
  unique.layer<-unique(missing$raster_nr[selection])
  for (k in 1:length(unique.layer)){
    print(k)
    layer <- rad$dat[[unique.layer[k]]]
    # assign CRS: 
    layer <- projectRasterDWD(layer,proj = "radolan")
    
    selection2<- which(missing$link_prec==unique.tar[i] & missing$raster_nr==unique.layer[k])
    unique.cells<-unique(missing$cellID_prec[selection2])
    ## to do: check individual cells for closest neighbouring cell (using distance dataframe)
    # loop through unique.cells: each cell within unique.cells has its own column within the distance matrix --> check the neighbouring cells for non-NA values
    # bind to data.frame??? 
    closest_prec <- c()
    for (l in 1:length(unique.cells)){
      cell_col <- paste0("",unique.cells[l],"")
      buffer <- set_units(30000, "m")
      
      # sort neighbouring cells of unique.cells according to their distance to the respective unique.cell and remove all neigbouring cells witch a distance > buffer (30 km)
      sorted_df <- dist_df %>%
        select(cell_id, all_of(cell_col)) %>%
        filter(.data[[cell_col]] <= buffer) %>%
        arrange(.data[[cell_col]])
      
      closest_cells <- sorted_df$cell_id
      
      # get precipitation for all cells within bounding box:
      extracted <-  (terra::extract(layer, closest_cells))
      colnames(extracted) <- "prec"
      # get non-NA precipitation of cell closest to site cell 
      prec <- extracted$prec[which(!is.na(extracted$prec))[1]]
      
      closest_prec <- rbind(closest_prec, prec)
    }
    
    extracted <- data.frame(prec = closest_prec[,1],
                            cellID_prec = unique.cells,
                            raster_nr = rep(unique.layer[k], length(unique.cells)),
                            link_prec = rep(unique.tar[i], length(unique.cells)))
    
    
    missing.data.prec<-rbind(missing.data.prec, extracted)
  }
}


# check for duplicate rows: 
sum(duplicated(missing.data.prec)) #--> 1287 duplicate rows_append()
missing.data.prec <- missing.data.prec[!duplicated(missing.data.prec), ]

# save data on extracted prec values
write.csv(missing.data.prec,"analysis_bees_diversity/data/data_weather/missing.data.prec.csv", row.names = FALSE)


## Check missing extracted precipitation data
## check how many NAs still exist --> for these no neighbouring cells within 30 km had non-NA values -> set to 0

missing.data.prec_NA <- which(is.na(missing.data.prec$prec)) # 2661 NAs

# replace missing precipitation values which cannot be replaced with neighbouring values within the 30km-buffer with 0
missing.data.prec <- missing.data.prec %>% 
  mutate(prec = ifelse(is.na(prec), 0, prec))


## 5.5.4 Merge filled with all.dat.temp.prec (extracted data on temperature and precipitation) -------

# merge neigbouring precipitation data with missing precipitation data --> 'filled precipitation data' --

missing_filled <- missing %>%
  left_join(missing.data.prec, by = c("cellID_prec", "raster_nr", "link_prec")) %>%
  mutate(prec = prec.y) %>%
  select(c(-"prec.y", -"prec.x"))


# merge 'filled precipitation data' with all.dat.temp.prec

all.dat.temp.prec <- all.dat.temp.prec %>%
  left_join(missing_filled, by = c("cellID_prec", "raster_nr", "link_prec")) %>%
  mutate(prec = prec.x) %>%
  mutate(trap = trap.x) %>%
  mutate(landscape = landscape.x) %>%
  select(c(-"prec.x", -"landscape.y", -"landscape.x", -"unique.ID", -"hour.y", -"trap.y",  -"trap.x"))

# check for duplicate rows: 
sum(duplicated(all.dat.temp.prec)) #--> 5776 duplicate rows_append()
all.dat.temp.prec <- all.dat.temp.prec[!duplicated(all.dat.temp.prec), ]

# replace NA-values in originally extracted prec-column with values from prec.y
all.dat.temp.prec <- all.dat.temp.prec %>%
  mutate(prec = coalesce(prec, prec.y)) %>%
  select(c(-"prec.y"))

# check if there a still NA precipitation values 
which(is.na(all.dat.temp.prec$prec))

# save data on extracted prec & temp values
write.csv(all.dat.temp.prec,"analysis_bees_diversity/data/data_weather/all.dat.temp.prec.csv", row.names = FALSE)


## 5.6 backtransform extracted temp and prec data into master data.frame ------------

# needed: 'data_samp_clim': Large List (2258 elements) including 2258 lists with each list corresponding to one row in the dataframe 'meta.trapyearseason'

meta.trapyearseason
data_samp_clim


### next steps:
#(iv) training data: run the selection procedure of best model constants (best settings for suitability approach)
#(v) testing data: compare the two modelling approaches (exposure days vs. suitability scores), accounting 
#     for co-linearity, non-linearity and different scales
# (v) look as seasonality effects



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

