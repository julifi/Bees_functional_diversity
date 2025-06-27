## Global Setting ####################################################
install.packages("rdwd")
install.packages("RCurl")


# BEFORE: run setting.R
getwd()


## Load Libraries ############################################################# 
library(rdwd); library(RCurl); library(dplyr); library(readxl); library(terra)
#rdwd::updateRdwd()



### 1. check available files on the dwd FTP server -------
# currently available files in a given folder:
rasterbase <- paste0(gridbase,"/hourly/hostrada")
ftp.files <- indexFTP("/air_temperature_mean", base=rasterbase, dir=tempdir())

# current index of all grid files (takes > 2 min, yields >30k charstrings >5MB):
#gridIndexNow <- indexFTP(base=gridbase, filename="grids")

## set reference raster
link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025040100-2025043023.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
#plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)

### 2. read nc files ------------------

## .nc files containing hourly temperature data for one month: 
# The name of the NetCDF file is formed as follows:
# parameter abbreviation}_{time resolution}_{process name version}_{variant}_{grid_info}_{time coverage}.nc
# (variant: BE - best estimate; grid_info: gn - native grid), e.g.
# tas_1hr_HOSTRADA-v1-0_BE_gn_1995010100-1995013123.nc

## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)
# --> 5th day, 2am (14 o'clock) = (5-1)*24+14 = 110
# --> xth day, y o'clock = (x-1)*24 + y

link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025040100-2025043023.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)

link <- "/hourly/hostrada/air_temperature_mean/tas_1hr_HOSTRADA-v1-0_BE_gn_2025030100-2025033123.nc"  #  5 MB
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=3)


### 3. load spatial data of TERENO sites --------------
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

sites_2010 <- dplyr::filter(sites, Year == 2010)
sites_2010 <- sites_2010[c("Trap", "lon", "lat")]
sites_2010 <- sites_2010[,c(3,2,1)]
sites_2010$lat <- as.numeric(sites_2010$lat)
sites_2010$lon <- as.numeric(sites_2010$lon)
sites_2010 <- as.data.frame(sites_2010)

# set reference raster: rad[[1]]
raster <- rad[[1]]

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


### 4. choose relavant raster files (hourly resolution) on temp & precipitation from .nc files -----
# depending on: exact sampling days, daytime

## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)
# --> 5th day, 2am (14 o'clock) = (5-1)*24+14 = 110
# --> xth day, y o'clock = (x-1)*24 + y

# load dat_all (prepared in script data_preparation.R)

# create raster stack for each sampling period containing all daylight raster within the sampling period (for Start and Endday: start/end at 12 o'clock)
dat_all[1,]$StartDate
dat_all[1,]$EndDate

### 5. compute suitability scores for wild-bee pollination for each sampling interval  -------

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


