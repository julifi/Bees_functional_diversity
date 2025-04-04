## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries ############################################################# 
library(raster)
library(terra)
library(sf)
library(tidyterra)
library(sf)
library(giscoR)
library(ggplot2)
library(reshape2)

### getting the coordinates 

coordinates <- read.csv2(paste0(datpath,"coordinates.csv"))
head(coordinates)

### getting the tiff

# Specify the path to your TIFF file
tiff_file <- "preidl_2018_sa_crop.tif"
r <- rast(tiff_file)
r

# Initialize an empty list to store the extracted data for each coordinate
extracted_data_list <- list()

# Loop through the rows of the coordinates data frame
for (i in 1:nrow(coordinates)) {
  coord <- data.frame(X = coordinates$X[i], Y = coordinates$Y[i])
  
  # Create a SpatVector from the coordinate
  point_x <- vect(coord, geom = c("X", "Y"), crs = "epsg:32632")
  point_x$trap <- coordinates$trap[i]
  
  # Create a buffer around the point
  buffer_circle <- buffer(point_x, width = 500)
  
  # Crop the raster to the buffer extent
  r_crop <- crop(r, buffer_circle, mask = FALSE)
  
  # Extract values from the cropped raster
  extracted_data <- values(r_crop)
  
  # Store the extracted data in the list
  extracted_data_list[[i]] <- data.frame(Coordinate = coordinates$trap[i], CTM_GER_2018 = extracted_data)
}

# Combine all the extracted data into a single data frame
seb_2018_500 <- do.call(rbind, extracted_data_list)