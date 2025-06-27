## Global Setting ####################################################
install.packages("rdwd")
install.packages("RCurl")
install.packages("lubridate")
install.packages("terra")
install.packages('dwdradar')

# BEFORE: run setting.R
getwd()


## Load Libraries ############################################################# 
library(rdwd); library(RCurl); library(dplyr); library(readxl); library(terra); library(lubridate); 
library(tidyr); library(dwdradar)
rdwd::updateRdwd()


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

# example data: 
link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025030100-2025033123.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=3)

# 1.2 Precipitation data: ASCII Format

# recent: 2005-06-01 until 2020-12-31
# example data: 
link <- "hourly/radolan/recent/bin/raa01-rw_10000-2501010000-dwd---bin.gz" # 25 mB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE) # dbin -> mode=wb
rad <- readDWD(file)
plotRadar(rad$dat, main=".binary RW", extent="rw", layer=1)

# historical: 2005-06-01 until today



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
# use projection of reference raster (ETRS89-extended / LCC Europe --> EPSG: 3034)
sites_proj <- project(sites_s, "EPSG:3034")
plot(sites_proj)

# extract grid cell IDs of sites 
cell_id <- cellFromXY(raster, crds(sites_proj))  

# add colum with cell_id to sites data.frame:
sites_id_2010 <- cbind(sites_2010, cell_id)


### 3. choose relevant raster files (hourly resolution) on temp & precipitation from .nc files  -----------------------
# depending on: exact sampling days, daytime

# read prepared sampling data:
data_sampling <- readRDS("analysis_bees_diversity/data/sampling_days_siteyseason.RData")
data_samp_clim <- data_sampling

# load metadata:
meta <- read.csv("analysis_bees_diversity/data/meta_sampling_days_siteyseason.csv")

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

## TEST data: 
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


# # create string with trap/site names in order to add column with site/trap names --> replicate each row for all site/traps
# traps <- do.call(paste, c(as.list(sites_2010$Trap), sep = ", "))

# refine daytime
# load data on daytime hours: 
daytime_hours <- read_excel("analysis_bees_diversity/data/data_raw/data_climate/daytime_hours.xlsx", sheet = "daytime_hours")
daytime_hours <- daytime_hours[c("month", "sunrise_hour", "sunset_hour")]
daytime_hours$sunrise_hour <- format(daytime_hours$sunrise_hour, "%H:%M:%S")
daytime_hours$sunset_hour <- format(daytime_hours$sunset_hour, "%H:%M:%S")


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
    
    data_samp_clim[[i]]$hour <- strptime(data_samp_clim[[i]]$hour, format = "%H:%M:%S") ## alternative: add time information to column 'date'
  }




## 4.1 extract climate data from raster
# select proper .nc file and within there: proper raster 
# year + month --> select .nc file
# day + start/end (+ daytime) --> select raster within .nc file


# 4.1.1 Temperature data
# get name of .nc file of interest
# tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc

#test_sampling_l$nc_temp <- NA

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

# 4.1.2 Precipitation data

##
##
# to be done
##
##
##


# 4.2 extract climate data from raster


# information on site of data.frame i in list data_samp_clim: 
meta[i,2]
# get cell number of respective site:
sites_id_2010$cell_id[sites_id_2010$Trap == meta[i,2]] 


#for(i in 1:length(data_samp_clim)){
for(i in 1:5){
  data_samp_clim[[i]]$temp <- NA
  
  for(j in 1:length(unique(data_samp_clim[[i]]$nc_temp))){
    link <- paste0("/hourly/hostrada/air_temperature_mean/",unique(data_samp_clim[[i]]$nc_temp)[j])
    file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
    rad <- readDWD(file) 
    
    for (k in 1:nrow(data_samp_clim[[i]])){
      if((data_samp_clim[[i]]$nc_temp[k] == unique(data_samp_clim[[i]]$nc_temp)[j]) == TRUE) {
        data_samp_clim[[i]]$temp[k] <- (terra::extract(raster, test_sampling_l$cell_id[[i]]))[[1]]
        }
    else{}
      }
    
  }

  case_when
  
  link <- paste0("/hourly/hostrada/air_temperature_mean/",data_samp_clim[[i]]$nc_temp)  #  5 MB
  file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
  rad <- readDWD(file) 
  test_sampling_l$temp[[i]] <- (terra::extract(raster, test_sampling_l$cell_id[[i]]))[[1]]
  
  # # check if data.frame covers spring or summer season
  # if(grepl('dates.spring', colnames(data_samp_clim[[i]]))[1] == TRUE){
  #   # get day: 
  #   x <- as.numeric(format(data_samp_clim[[i]]$dates.spring, "%d"))
  #   # get hour:
  #   y <- as.numeric(format(data_samp_clim[[i]]$hour, "%H"))
  # }else{
  #   # get day: 
  #   x <- as.numeric(format(data_samp_clim[[i]]$dates.summer, "%d"))
  #   # get hour:
  #   y <- as.numeric(format(data_samp_clim[[i]]$hour, "%H"))
  # }
  # raster_nr <- (x-1)*24+1 + y
  # data_samp_clim[[i]]$raster_nr<- raster_nr
}

test_sites_id_2010test_sampling_l$temp <- NA
for(i in 1:nrow(test_sampling_l)){
  link <- paste0("/hourly/hostrada/air_temperature_mean/",test_sampling_l$nc_temp[[i]])  #  5 MB
  file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
  rad <- readDWD(file) 
  test_sampling_l$temp[[i]] <- (terra::extract(raster, test_sampling_l$cell_id[[i]]))[[1]]
}


# test_sites_id_2010test_sampling_l$temp <- NA
# for(i in 1:nrow(test_sampling_l)){
#   link <- paste0("/hourly/hostrada/air_temperature_mean/",test_sampling_l$nc_temp[[i]])  #  5 MB
#   file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
#   rad <- readDWD(file) 
#   test_sampling_l$temp[[i]] <- (terra::extract(raster, test_sampling_l$cell_id[[i]]))[[1]]
# }



## 5. compute suitability scores for wild-bee pollination for each sampling interval  -------

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

### 6. load in predictors and response variables and prepare them  -------

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

library(ggplot2)

# look at abundance trend over time... 
ggplot(data= meta.trapyearseason, aes(x=year,))

colnames(meta.trapyearseason)
str(meta.trapyearseason)
# IDEA: water (rivers) might be a good additional predictor - we could explore this at a later point in time

### 8. start modelling the grid for the suitability score and identify the best solution  -------



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