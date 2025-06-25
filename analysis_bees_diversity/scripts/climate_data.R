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


### 4. choose relavant raster files (hourly resolution) on temp & precipitation from .nc files 
# depending on: exact sampling days, daytime

## per .nc file: nr of layers = 24*nr of days (30days: 720; 31 days: 744; 38 days: 672; 29 days: 696)
# --> 5th day, 2am (14 o'clock) = (5-1)*24+14 = 110
# --> xth day, y o'clock = (x-1)*24 + y

# load dat_all (prepared in script data_preparation.R)

# create raster stack for each sampling period containing all daylight raster within the sampling period (for Start and Endday: start/end at 12 o'clock)
dat_all[1,]$StartDate
dat_all[1,]$EndDate


